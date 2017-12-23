(ns mad-max.controller
  (:require [mad-max.arena :as mm-arena]
            [mad-max.mm-player :as mm-player]
            [mad-max.indestructible-wall :as ind-wall]
            [mad-max.util :as util]
            [mad-max.arena :as arena]
            [mad-max.actions :as actions]
            [mad-max.renderer :as renderer]
            [mad-max.server :as server]
            [mad-max.mm-bullet :as mm-bullet]
            [mad-max.holy-grenade :as holy-grenade]
            [mad-max.client :as mm-client]
            [mad-max.health-powerup :as mm-health-powerup]
            [mad-max.grenade :as mm-grenade]))

(def health-powerup-chance 1000)
(def max-player-name-length 8)

(def arena-id (ref 0))
(def arenas (ref {}))
(def entity-id (ref 0))
(def entities (ref {}))

(def clients (ref {}))

; TODO tie refresh thread to arena
; TODO tie clients to arenas
; TODO use two above to send clients the screen

(def special-entity-ids (ref {}))

(def actions-thread (atom nil))
(def send-frames-thread (atom nil))
(def world-thread (atom nil))

(def dir-to-movement {:up    [dec :y]
                      :down  [inc :y]
                      :left  [dec :x]
                      :right [inc :x]})

(def dir-to-opposite {:up :down
                      :down :up
                      :left :right
                      :right :left})

(defn change-cell [coords direction]
  (update coords
          (second (dir-to-movement direction))
          (first (dir-to-movement direction))))

(defn stop-actions-thread []
  (if (nil? @actions-thread)
    (util/debug-print "No action thread running, can't stop")
    (do
      (.stop @actions-thread)
      (reset! actions-thread nil))))

(defn stop-world-thread []
  (if (nil? @world-thread)
    (util/debug-print "No action thread running, can't stop")
    (do
      (.stop @world-thread)
      (reset! world-thread nil))))

(defn stop-send-frames-thread []
  (if (nil? @send-frames-thread)
    (util/debug-print "No send frames thread running, can't stop")
    (do
      (.stop @send-frames-thread)
      (reset! send-frames-thread nil))))
;; TODO DRY violation: thread functions are almost the same

(defn drain-actions []
  (loop [])
  (if (empty? @actions/queue)
    nil
    (do (actions/dequeue)
        (recur))))

(defn reset-all-state []
  (dosync
    (ref-set arena-id 0)
    (ref-set arenas {})
    (ref-set entity-id 0)
    (ref-set entities {})
    (ref-set clients {})
    (ref-set special-entity-ids {}))
  (drain-actions)
  (stop-send-frames-thread)
  (stop-world-thread)
  (stop-actions-thread))

(defn add-arena [arena]
  (alter arenas assoc @arena-id arena)
  (alter arena-id inc)
  (dec @arena-id))

(defn add-entity
  ([entity a-id]
   (alter entities assoc @entity-id (merge entity {:arena-id a-id}))
   (alter entity-id inc)
   (dec @entity-id))
  ([entity]
   (alter entities assoc @entity-id entity)
   (alter entity-id inc)
   (dec @entity-id)))

(defn add-special-entity-id [entity e-id]
  (alter special-entity-ids assoc (get entity :type) e-id))

(defn get-special-entity-id [entity-type]
  (get @special-entity-ids entity-type))

(defn add-connected-client [client p-id a-id]
  (util/debug-print "Adding client " client)
  (util/debug-print "  player-id: " p-id)
  (util/debug-print "  arena-id: " a-id)
  (alter clients merge {client {:player-id p-id :arena-id a-id :window {:width 0 :height 0}}})
  (alter arenas update-in [a-id :clients] conj client)
  (alter arenas update-in [a-id :player-ids] conj p-id))

(defn connect-client-to-arena [client a-id]
  (let [p-id ((@clients client) :player-id)]
    (alter clients update client merge {:arena-id a-id :state :in-action})
    (alter arenas update-in [a-id :clients] conj client)
    (alter arenas update-in [a-id :player-ids] conj p-id)
    (alter entities update p-id assoc :arena-id a-id)))

(defn remove-entity-from-cell [a-id cell e-id]
  (util/debug-print "Removing entity at: " cell)
  (util/debug-print "Entity id: " e-id)
  (alter arenas update-in [a-id :entities-map cell] disj e-id)
  (alter entities update e-id dissoc :cell))

(defn remove-entity [e-id]
  (when-not (contains? @special-entity-ids ((@entities e-id) :type))
    (let [{:keys [cell arena-id]} (@entities e-id)]
      (util/debug-print "Removing entity: " e-id)
      (remove-entity-from-cell arena-id cell e-id)
      (alter entities dissoc e-id))))

(defn player-by-id [p-id]
  (get @entities p-id {}))

(defn player-by-client [client]
  (player-by-id ((@clients client) :player-id)))

(defn take-a-hit [player-id hitting-entity-id]
  (alter entities update-in [player-id :health] - ((@entities hitting-entity-id) :damage))
  (if (<= ((player-by-id player-id) :health) 0)
    (alter entities update player-id merge {:destructible? false :alive? false})))

(defn heal [player-id health-powerup-id]
  (alter entities update-in [player-id :health] + ((@entities health-powerup-id) :heal))
  (remove-entity health-powerup-id))

(defn entity-stop [e-id]
  (alter entities update e-id assoc :velocity {:x 0 :y 0}))

(declare make-and-add-bullet)
(declare place-entity-at-cell)

(defn cell-valid? [cell a-id]
  (let [{:keys [width height]} ((@arenas a-id) :dimensions)
        {:keys [x y]} cell]
    ;(and (<= 0 x (dec width))
    ;     (<= 0 y (dec height)))
    (and (<= 1 x (- width 2))
         (<= 1 y (- height 2)))))

(defn explode-grenade [grenade-id]
  (dosync
    (let [{:keys [player-id arena-id real-cell direction]} (@entities grenade-id)
          {:keys [color]} (@entities player-id)
          cell (mm-grenade/real-cell-to-cell real-cell)]
      (doseq [bullet-direction [:up :down :left :right]]
        (let [bullet-cell (change-cell cell bullet-direction)
              bullet-cell-corrected (if (cell-valid? bullet-cell arena-id)
                                      bullet-cell
                                      (change-cell bullet-cell (dir-to-opposite direction)))]
          (when (cell-valid? bullet-cell-corrected arena-id)
            (let [bullet (mm-bullet/make-bullet :direction bullet-direction :player-id player-id :real-cell bullet-cell-corrected :arena-id arena-id)
                  bullet-id (add-entity bullet arena-id)]
              (place-entity-at-cell arena-id bullet-cell-corrected bullet-id))))))))

;(defn move-back-1-cell [e-id]
;  (let [cell (get-in @entities [e-id :cell])
;        opposite-dir (-> (get-in @entities [e-id :direction]) dir-to-opposite)]
;    (alter entities update-in [e-id :cell] #(change-cell % opposite-dir))))

(defmulti collision (fn [entity-id-1 entity-id-2]
                      (let [[entity-1 entity-2] (map @entities [entity-id-1 entity-id-2])]
                        (vector (entity-1 :type) (entity-2 :type)))))

(defmethod collision [:bullet :indestructible-wall] [bullet-id ind-wall-id]
  (remove-entity bullet-id))

(defmethod collision [:indestructible-wall :bullet] [ind-wall-id bullet-id]
  (collision bullet-id ind-wall-id))

(defmethod collision [:bullet :player] [bullet-id player-id]
  (if ((@entities player-id) :destructible?)
    (take-a-hit player-id bullet-id)
    (remove-entity bullet-id)))

(defmethod collision [:player :bullet] [player-id bullet-id]
  (collision bullet-id player-id))

(defmethod collision [:health-powerup :bullet] [health-powerup-id bullet-id])

(defmethod collision [:bullet :health-powerup] [bullet-id health-powerup-id]
  (collision health-powerup-id bullet-id))

(defmethod collision [:health-powerup :player] [health-powerup-id player-id]
  (heal player-id health-powerup-id))

(defmethod collision [:player :health-powerup ] [player-id health-powerup-id]
  (collision health-powerup-id player-id))

(defmethod collision [:bullet :bullet] [bullet-id-1 bullet-id-2])

(defmethod collision [:player :player] [player-id-1 player-id-2])

(defmethod collision [:grenade :player] [grenade-id player-id]
  (when ((@entities player-id) :destructible?)
    (take-a-hit player-id grenade-id)
    (explode-grenade grenade-id)
    (remove-entity grenade-id)))

(defmethod collision [:player :grenade] [player-id grenade-id]
  (collision grenade-id player-id))

(defmethod collision [:grenade :grenade] [grenade-id-1 grenade-id-2])
(defmethod collision [:grenade :bullet] [grenade-id bullet-id])
(defmethod collision [:bullet :grenade] [bullet-id grenade-id])
(defmethod collision [:grenade :health-powerup] [grenade-id health-powerup-id])
(defmethod collision [:health-powerup :grenade] [health-powerup-id grenade-id])
(defmethod collision [:grenade :indestructible-wall] [grenade-id indestructible-wall-id]
  (explode-grenade grenade-id)
  (remove-entity grenade-id))

(defmethod collision [:indestructible-wall :grenade] [indestructible-wall-id grenade-id]
  (collision grenade-id indestructible-wall-id))

(defmethod collision [:default :default] [parameters])

(defn collision? [a-id cell]
  (> (count (get-in @arenas [a-id :entities-map cell])) 1))

(defn process-collision [a-id cell e-id]
  (let [entitiy-ids-at-cell (get-in @arenas [a-id :entities-map cell])
        entitiy-ids-at-cell-all-but (disj entitiy-ids-at-cell e-id)
        collision-map (for [moved [e-id]
                            bumped-into entitiy-ids-at-cell-all-but]
                        [moved bumped-into])]
    (util/debug-print "Collision map: " collision-map)
    (doseq [[e-id-1 e-id-2] collision-map]
      (collision e-id-1 e-id-2))))


(defn place-entity-at-cell [a-id cell e-id]
  (alter arenas update-in [a-id :entities-map cell] conj e-id)
  (alter entities update e-id assoc :cell cell)
  (when (collision? a-id cell)
    (util/debug-print "Collision detected at cell: " cell)
    (process-collision a-id cell e-id)))

(defn move-from-cell-to-cell [a-id old-cell new-cell e-id]
  (when-not (= old-cell new-cell)
    (remove-entity-from-cell a-id old-cell e-id)
    (place-entity-at-cell a-id new-cell e-id)))

(defn choose-arena []
  (if (< @arena-id 1)
    (add-arena (mm-arena/make-arena {} (get-special-entity-id :indestructible-wall)))
    0))

(defn make-player [client]
  (dosync
    (let [player (mm-player/make-player)
          player-id (add-entity player)]
      player-id)))

(defn add-player-to-arena [client]
  (dosync
    (let [chosen-arena-id (choose-arena)
          chosen-arena (get @arenas chosen-arena-id)
          player-id ((@clients client) :player-id)]
      (connect-client-to-arena client chosen-arena-id)
      (place-entity-at-cell chosen-arena-id {:x 1 :y 1} player-id))))

(defn remove-client [client]
  (dosync
    (let [{:keys [arena-id]} (@clients client)]
      (if ((player-by-client client) :alive?)
        (take-a-hit ((@clients client) :player-id) (get-special-entity-id :holy-grenade)))
      (alter arenas update-in [arena-id :clients] disj client)
      (alter clients dissoc client))))

(defn remove-player [p-id]
  (dosync
    (let [{:keys [arena-id]} (player-by-id p-id)]
      (alter arenas update-in [arena-id :player-ids] #(vec (remove #{p-id} %)))
      (remove-entity p-id))))

(defn make-and-add-bullet [client]
  (when ((player-by-client client) :alive?)
    (dosync
      (let [{:keys [player-id arena-id]} (@clients client)
            {:keys [direction color cell]} (@entities player-id)
            bullet-cell (change-cell cell direction)
            bullet (mm-bullet/make-bullet :direction direction :player-id player-id :real-cell bullet-cell :arena-id arena-id)
            bullet-id (add-entity bullet arena-id)]
        (place-entity-at-cell arena-id bullet-cell bullet-id)))))

(defn render-and-send-arena! [a-id]
  (let [arena (get @arenas a-id)
        render (renderer/render-arena arena @entities)
        arena-clients (seq (get arena :clients))]
    ;(util/debug-print "Sending render: " render)
    (doseq [client arena-clients]
      (let [{:keys [width height]} (get-in @clients [client :window])
            resized-screen (renderer/resize-screen render width height)
            frame (renderer/screen-to-full-frame resized-screen)]
        (try
          (server/send-full-frame client frame)
          (catch Exception e))))))

(defn upmerge-map-ref [_ref key data]
  (dosync
    (util/debug-print "Updating: " @_ref)
    (util/debug-print "  Key: " key)
    (util/debug-print "  Data: " data)
    (alter _ref update key merge data)))

(declare initialize)

(defn make-and-add-health-powerup [a-id cell]
  (let [powerup (mm-health-powerup/make-health-powerup :arena-id a-id)
        powerup-id (add-entity powerup a-id)]
    (place-entity-at-cell a-id cell powerup-id)))

(defn passable? [a-id cell]
  (let [arena (@arenas a-id)
        entities-ids-in-cell (get-in arena [:entities-map cell])
        entities-in-cell (map (partial get @entities) entities-ids-in-cell)]
    (util/debug-print "Checking if passable:")
    (util/debug-print "Cell: " cell)
    (util/debug-print "Entities: " entities-in-cell)
    (every? :passable? entities-in-cell)))

(defn move-player [client direction]
  (when ((player-by-client client) :alive?)
    (let [{:keys [player-id arena-id]} (@clients client)
          current-cell (get-in @entities [player-id :cell])
          new-cell (change-cell current-cell direction)]
      (dosync
        (if (passable? arena-id new-cell)
          (move-from-cell-to-cell arena-id current-cell new-cell player-id))
        (alter entities update player-id merge {:direction direction})))))

(defmulti heartbeat (fn [entity-id] (get-in @entities [entity-id :type])))

(defmethod heartbeat :indestructible-wall [_])
(defmethod heartbeat :holy-grenade [_])
(defmethod heartbeat :health-powerup [_])

(defmethod heartbeat :grenade [grenade-id]
  (let [grenade (@entities grenade-id)
        {:keys [real-cell velocity arena-id cell]} grenade
        new-real-cell (merge-with + real-cell velocity)
        new-cell (mm-grenade/real-cell-to-cell new-real-cell)]
    (dosync
      (alter entities update grenade-id merge {:real-cell new-real-cell})
      (alter entities update-in [grenade-id :time-to-explode] dec)
      (alter entities update-in [grenade-id :time-to-drop] dec)
      (when (<= ((@entities grenade-id) :time-to-drop) 0)
        (entity-stop grenade-id))
      (if (<= ((@entities grenade-id) :time-to-explode) 0)
        (do
          (explode-grenade grenade-id)
          (remove-entity grenade-id))
        (move-from-cell-to-cell arena-id cell new-cell grenade-id)))))

(defmethod heartbeat :player [p-id]
  (when (not ((player-by-id p-id) :alive?))
    (dosync
      (if (> ((player-by-id p-id) :time-to-vanish) 0)
        (alter entities update-in [p-id :time-to-vanish] dec)
        (remove-player p-id)))))

(defmethod heartbeat :bullet [bullet-id]
  (let [bullet (@entities bullet-id)
        {:keys [real-cell velocity arena-id cell]} bullet
        new-real-cell (merge-with + real-cell velocity)
        new-cell (mm-bullet/real-cell-to-cell new-real-cell)]
    (dosync
      (alter entities update bullet-id merge {:real-cell new-real-cell})
      (move-from-cell-to-cell arena-id cell new-cell bullet-id))))

(defn arena-heartbeat [a-id]
  (let [r (rand-int health-powerup-chance)]
    (when (zero? r)
      (dosync
        (let [entities-map ((@arenas a-id) :entities-map)
              empty-cells (filter #(empty? (second %)) entities-map)]
          (when-not (empty? empty-cells)
            (let [empty-cell (first (rand-nth empty-cells))]
              (make-and-add-health-powerup a-id empty-cell))))))))

(defn execute-heartbeat []
  (doseq [e-id (keys @entities)]
    (heartbeat e-id))
  (doseq [a-id (keys @arenas)]
    (arena-heartbeat a-id)))

(defn make-and-add-client [client p-id]
  (dosync
    (alter clients assoc client (mm-client/make-client p-id)))
  (server/send-full-frame client "Enter your name to continue: "))

(defn make-and-add-grenade [client]
  (when ((player-by-client client) :alive?)
    (dosync
      (let [{:keys [player-id arena-id]} (@clients client)
            {:keys [direction color cell]} (@entities player-id)
            grenade-cell (change-cell cell direction)
            grenade (mm-grenade/make-grenade :direction direction :player-id player-id :real-cell grenade-cell :arena-id arena-id)
            grenade (add-entity grenade arena-id)]
        (place-entity-at-cell arena-id grenade-cell grenade)))))


(def char-to-action-and-command
  (merge
    (apply hash-map
      (interleave
        [:arrow-up :arrow-down :arrow-left :arrow-right]
        (map (partial hash-map :type :move-player :direction)
             [:up :down :left :right])))
    {\space {:type :shoot}}
    {\z {:type :throw-grenade}}))

(defn dispatch-client-char-input [action]
  (util/debug-print "char-input action: " action)
  (when (= ((@clients (action :client)) :state) :in-action)
    (let [sub-action (merge action (char-to-action-and-command (action :input)))
          action-type (get sub-action :type)]
      (case action-type
        :move-player (move-player (sub-action :client) (sub-action :direction))
        :shoot (make-and-add-bullet (sub-action :client))
        :throw-grenade (make-and-add-grenade (sub-action :client))
        nil))))

(defn dispatch-client-line-input [action]
  (util/debug-print "line-input action: " action)
  (util/debug-print "client state: " ((@clients (action :client)) :state))
  (when (= ((@clients (action :client)) :state) :entering-name)
    (let [player-name (->> (action :input) (take max-player-name-length) (apply str))
          player-id ((@clients (action :client)) :player-id)]
      (when-not (empty? player-name)
        (util/debug-print "Connecting client...")
        (server/send-no-echo (action :client))
        (dosync
          (alter entities update player-id assoc :name player-name))
        (actions/enqueue {:type :connect-client-to-arena
                          :client (action :client)})))))

(defn dispatch-action [action]
  (let [action-type (get action :type)]
    (case action-type
      :client-connect (make-and-add-client (action :client) (make-player (action :client)))
      :client-disconnect (remove-client (action :client))
      :connect-client-to-arena (add-player-to-arena (action :client))
      :print (println (action :message))
      :update-client (upmerge-map-ref clients (action :client) (action :data))
      :stop-controller (reset-all-state)
      :input-char (dispatch-client-char-input action)
      :input-line (dispatch-client-line-input action)
      ;:move-player (move-player (action :client) (action :direction))
      ;:shoot (make-and-add-bullet (action :client))
      :heartbeat (execute-heartbeat)
      (util/debug-print "Unknown action: " action))))

(defn process-actions []
  (loop []
    (if (empty? @actions/queue)
      nil
      (do (dispatch-action (actions/dequeue))
          (recur)))))

(defn send-all-frames! []
  (doseq [a-id (keys @arenas)]
    (render-and-send-arena! a-id)))

(defn world-heart-beat []
  (actions/enqueue {:type :heartbeat}))

(defn actions-loop []
  (loop []
    (process-actions)
    (Thread/sleep 10)
    (recur)))

(defn world-loop []
  (loop []
    (world-heart-beat)
    (Thread/sleep 10)
    (recur)))

(defn send-frames-loop []
  (loop []
    (send-all-frames!)
    (Thread/sleep 50)
    (recur)))

(defn start-send-frames-thread []
  (if (nil? @send-frames-thread)
    (reset! send-frames-thread
      (doto
        (Thread. send-frames-loop)
        (.setDaemon true)
        (.start)))
    (util/debug-print "Send frames thread's already running")))

(defn start-actions-thread []
  (if (nil? @actions-thread)
    (reset! actions-thread
      (doto
        (Thread. actions-loop)
        (.setDaemon true)
        (.start)))
    (util/debug-print "Action thread's already running")))

(defn start-world-thread []
  (if (nil? @world-thread)
    (reset! world-thread
      (doto
        (Thread. world-loop)
        (.setDaemon true)
        (.start)))
    (util/debug-print "World thread's already running")))

(defn initialize []
  (dosync
    (let [ind-wall (ind-wall/make-indestructible-wall)
          ind-wall-id (add-entity ind-wall nil)
          h-grenade (holy-grenade/make-holy-grenade)
          h-grenade-id (add-entity h-grenade nil)]
      (add-special-entity-id ind-wall ind-wall-id)
      (add-special-entity-id h-grenade h-grenade-id)))
  (start-actions-thread)
  (start-send-frames-thread)
  (start-world-thread))

(defn restart []
  (reset-all-state)
  (initialize))

(defn print-debug-info []
  (util/debug-print "Clients: " @clients)
  (util/debug-print "Arena 0: " (@arenas 0))
  (util/debug-print "Entities: " @entities))
