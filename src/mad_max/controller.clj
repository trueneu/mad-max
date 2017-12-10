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
            [mad-max.holy-grenade :as holy-grenade]))

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

(def dir-to-movement {:up [dec :y]
                      :down [inc :y]
                      :left [dec :x]
                      :right [inc :x]})

(defn change-cell [coords direction]
  (update coords (second (dir-to-movement direction))
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

(defn add-entity [entity a-id]
  (alter entities assoc @entity-id (merge entity {:arena-id a-id}))
  (alter entity-id inc)
  (dec @entity-id))

(defn add-special-entity-id [entity e-id]
  (alter special-entity-ids assoc (get entity :type) e-id))

(defn get-special-entity-id [entity-type]
  (get @special-entity-ids entity-type))

(defn add-connected-client [client p-id a-id]
  (util/debug-print "Adding client " client)
  (util/debug-print "  player-id: " p-id)
  (util/debug-print "  arena-id: " a-id)
  (alter clients assoc client {:player-id p-id :arena-id a-id :window {:width 0 :height 0}})
  (alter arenas update-in [a-id :clients] conj client)
  (alter arenas update-in [a-id :player-ids] conj p-id))

(defn remove-entity-from-cell [a-id cell e-id]
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
  (remove-entity hitting-entity-id)
  (if (<= ((player-by-id player-id) :health) 0)
    (alter entities update player-id merge {:destructible? false :alive? false})))


(defmulti collision (fn [entity-id-1 entity-id-2]
                      (let [[entity-1 entity-2] (map @entities [entity-id-1 entity-id-2])]
                        (vector (entity-1 :type) (entity-2 :type)))))

(defmethod collision [:bullet :indestructible-wall] [bullet-id ind-wall-id]
  (remove-entity bullet-id))

(defmethod collision [:indestructible-wall :bullet] [ind-wall-id bullet-id]
  (collision bullet-id ind-wall-id))

(defmethod collision [:bullet :player] [bullet-id player-id]
  (if ((@entities player-id) :destructible?)
    (take-a-hit player-id bullet-id)))

(defmethod collision [:player :bullet] [player-id bullet-id]
  (collision bullet-id player-id))

(defmethod collision [:bullet :bullet] [bullet-id-1 bullet-id-2])

(defmethod collision [:player :player] [player-id-1 player-id-2])

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

(defn make-and-add-player [client]
  (dosync
    (let [chosen-arena-id (choose-arena)
          chosen-arena (get @arenas chosen-arena-id)
          player (mm-player/make-player)
          player-id (add-entity player chosen-arena-id)]
      (util/debug-print "Making and adding player:")
      (util/debug-print "chosen-arena-id: " chosen-arena-id)
      (util/debug-print "player: " player)

      (add-connected-client client player-id chosen-arena-id)
      (util/debug-print "Clients now: " @clients)
      (place-entity-at-cell chosen-arena-id {:x 1 :y 1} player-id)

      (util/debug-print "resulting arena: " chosen-arena)
      player)))

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

(defn execute-heartbeat []
  (doseq [e-id (keys @entities)]
    (heartbeat e-id)))

(defn dispatch-action [action]
  (let [action-type (get action :type)]
    (case action-type
      :client-connect (make-and-add-player (action :client))
      :client-disconnect (remove-client (action :client))
      :print (println (action :message))
      :update-client (upmerge-map-ref clients (action :client) (action :data))
      :stop-controller (reset-all-state)
      :move-player (move-player (action :client) (action :direction))
      :shoot (make-and-add-bullet (action :client))
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
