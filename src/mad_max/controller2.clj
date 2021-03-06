(ns mad-max.controller2
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
            [mad-max.grenade :as mm-grenade]
            [mad-max.game :as mm-game]
            [mad-max.heartbeats :as heartbeats]
            [mad-max.cells :as cells]
            [mad-max.entities :as entities]
            [mad-max.collisions :as collisions]))

(def game-debug (atom nil))

(def max-name-length 10)

(def default-names ["Greg" "Ben" "Bob" "Bill" "Rose" "Alice" "Jane" "Ann"])
(def player-colors [:black :red :green :yellow :blue :magenta :cyan :white])

(def next-player-color (apply hash-map (concat (interleave player-colors (rest player-colors))
                                               (list (last player-colors) (first player-colors)))))

(defn add-arena [game arena]
  (-> game
      (assoc-in [:arenas (game :arena-id)] arena)
      (update :arena-id inc)))

(defn add-client [game client client-connection]
  (-> game
      (assoc-in [:clients client-connection] client)))

(defn remove-client [game client-connection]
  (util/debug-print "Rmvng client")
  (-> game
      (update :clients dissoc client-connection)))

(defn init-game [game]
  (let [ind-wall (ind-wall/make-indestructible-wall)
        h-grenade (holy-grenade/make-holy-grenade)]
    (reduce entities/add-special-entity game [ind-wall h-grenade])))

(defn dispatch-client-line-input [game client-connection input]
  (case (get-in game [:clients client-connection :state])
    :sent-name-prompt (let [name (if (empty? input)
                                   (rand-nth default-names)
                                   (subs input 0 (min max-name-length (count input))))]
                        (server/send-no-echo client-connection)
                        (actions/enqueue
                          {:type              :make-player
                           :name              name
                           :client-connection client-connection})
                        (assoc-in game [:clients client-connection :state] :in-action))
    game))

(def char-to-action-and-command
  (merge
    (apply hash-map
           (interleave
             [:arrow-up :arrow-down :arrow-left :arrow-right]
             (map (partial hash-map :type :move-player :direction)
                  [:up :down :left :right])))
    {\space {:type :shoot}}
    {\z {:type :throw-grenade}}))

(defn passable? [game arena-id cell]
  (let [entities-ids-in-cell (get-in game [:arenas arena-id :entities-map cell])
        entities-in-cell (map (partial get (game :entities)) entities-ids-in-cell)]
    (every? :passable? entities-in-cell)))

(defn move-player [game client-connection direction]
  (let [player-id (get-in game [:clients client-connection :player-id])
        player (get-in game [:entities player-id])
        arena-id (player :arena-id)
        current-cell (player :cell)
        new-cell (cells/change-cell current-cell direction)]
    (if (and (player :alive?) (passable? game arena-id new-cell))
      (-> game
          (cells/remove-entity-from-cell player-id)
          (cells/place-entity-at-cell player-id new-cell)
          (assoc-in [:entities player-id :direction] direction))
      game)))

(defn make-grenade [game client-connection]
  (let [{:keys [player-id arena-id]} (get-in game [:clients client-connection])
        {:keys [direction color cell alive?]} (get-in game [:entities player-id])
        grenade-cell (cells/change-cell cell direction)
        grenade (mm-grenade/make-grenade arena-id :direction direction :player-id player-id :real-cell grenade-cell :color color)
        grenade-id (game :entity-id)]
    (if alive?
      (->
        game
        (entities/add-entity grenade)
        (cells/place-entity-at-cell grenade-id grenade-cell))
      game)))

(defn make-bullet [game client-connection]
  (let [{:keys [player-id arena-id]} (get-in game [:clients client-connection])
        {:keys [direction color cell alive?]} (get-in game [:entities player-id])
        bullet-cell (cells/change-cell cell direction)
        bullet (mm-bullet/make-bullet arena-id :direction direction :player-id player-id :real-cell bullet-cell :color color)
        bullet-id (game :entity-id)]
    (if alive?
      (->
        game
        (entities/add-entity bullet)
        (cells/place-entity-at-cell bullet-id bullet-cell))
      game)))

(defn dispatch-client-char-input [game client-connection input]
  (let [player-id (get-in game [:clients client-connection :player-id] nil)
        player (get-in game [:entities player-id] nil)]
    (if (and player (player :alive?))
      (case (get-in game [:clients client-connection :state])
        :in-action (do
                     (let [action (char-to-action-and-command input)
                           action-type (get action :type)]
                       (case action-type
                         :move-player (move-player game client-connection (action :direction))
                         :shoot (make-bullet game client-connection)
                         :throw-grenade (make-grenade game client-connection)
                         game)))
        game)
      game)))

(defn choose-arena [game]
  (if (> (game :arena-id) 0)
    0
    nil))

(defn make-arena [game]
  (->> (arena/make-arena {} (entities/special-entity-id game :indestructible-wall))
       (add-arena game)))

(defn make-arena-if-needed [game]
  (let [chosen-arena-id (choose-arena game)]
    (if chosen-arena-id
      game
      (make-arena game))))

(defn make-player [game client-connection name]
  (let [g (make-arena-if-needed game)
        chosen-arena-id (choose-arena g)
        chosen-arena (get-in g [:arenas chosen-arena-id])
        unoccupied-cell (mm-arena/choose-unoccupied-cell chosen-arena)
        last-used-player-color (get-in g [:arenas chosen-arena-id :last-used-player-color])
        player-color (next-player-color last-used-player-color)
        player (mm-player/make-player name chosen-arena-id client-connection
                                      :color player-color
                                      :direction (rand-nth util/directions))

        player-id (g :entity-id)]
    (-> g
        (entities/add-entity player)
        (cells/place-entity-at-cell player-id unoccupied-cell)
        (update-in [:arenas chosen-arena-id :player-ids] conj player-id)
        (update-in [:arenas chosen-arena-id :clients] conj client-connection)
        (update-in [:arenas chosen-arena-id] assoc :last-used-player-color player-color)
        (update-in [:clients client-connection] merge {:player-id player-id
                                                       :arena-id  chosen-arena-id}))))

(defn dispatch-action [game action]
  (let [action-type (get action :type)]
    (case action-type
      :client-connect (add-client game (mm-client/make-client) (action :client-connection))
      ; FIXME make client-disconnect a clean separate function
      :client-disconnect (let [player-id (get-in game [:clients (action :client-connection) :player-id])
                               player-alive? (get-in game [:entities player-id :alive?] false)
                               g (remove-client game (action :client-connection))]
                           (if player-alive?
                             (update-in g [:entities player-id] mm-player/insta-death)
                             g))
      :make-player (make-player game (action :client-connection) (action :name))
      ;:print (println (action :message))
      :update-client (update-in game [:clients (action :client-connection)] merge (action :data))
      :stop-controller (assoc game :destroy true)
      :input-char (dispatch-client-char-input game (action :client-connection) (action :input))
      :input-line (dispatch-client-line-input game (action :client-connection) (action :input))
      ;:move-player (move-player (action :client) (action :direction))
      ;:shoot (make-and-add-bullet (action :client))
      (do (util/debug-print "Unknown action: " action)
          game))))

(defn process-events [game]
  (loop [g game]
    (if (actions/q-empty?)
      g
      (recur (dispatch-action g (actions/dequeue))))))

(defn heartbeat-entities [game]
  (reduce
    (fn [g entity-id]
      (heartbeats/heartbeat g entity-id))
    game
    (keys (game :entities))))

(defn heartbeat-arenas [game]
  (reduce
    (fn [g arena-id]
      (heartbeats/arena-heartbeat g arena-id))
    game
    (keys (game :arenas))))

(defn process-collisions [game]
  (reduce
    (fn [g arena-id]
      (collisions/detect-and-process g arena-id))
    game
    (keys (game :arenas))))

(defn process-game [game]
  (->
    game
    (heartbeat-entities)
    (process-collisions)
    (heartbeat-arenas)))

(defn render-arenas [game]
  (reduce
    (fn [g [arena-id arena]]
      (assoc-in g [:arenas arena-id :render] (renderer/render-arena arena (game :entities))))
    game
    (game :arenas)))

(defn send-arena-render! [game client-connection]
  (let [client (get-in game [:clients client-connection])
        arena-id (client :arena-id)
        render (get-in game [:arenas arena-id :render])]
    (let [{:keys [width height]} (client :window)
          resized-screen (renderer/resize-screen render width height)
          frame (renderer/screen-to-full-frame resized-screen)]
      (try
        (server/send-full-frame client-connection frame)
        (catch Exception e)))))

(defn send-renders! [game]
  (reduce
    (fn [g [client-connection client]]
      (case (client :state)
        :just-connected (do
                          (server/send-full-frame client-connection "Enter your name to continue: ")
                          (update-in g [:clients client-connection] assoc :state :sent-name-prompt))
        :sent-name-prompt g
        :in-action (do
                     (send-arena-render! g client-connection)
                     g)
        (do (util/debug-print "Unknown client state: " (client :state))
            g)))
    game (game :clients)))

(defn main-loop []
  (loop [game (-> (mm-game/make-game) init-game)
         telnet-server (server/start-server)]
    (reset! game-debug game)
    (if (game :destroy)
      (do
        (server/stop-server telnet-server)
        nil)
      (do
        (Thread/sleep 10)
        (recur
          (-> game
              process-game
              process-events
              render-arenas
              send-renders!)
          telnet-server)))))
