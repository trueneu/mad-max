(ns mad-max.controller
  (:require [mad-max.arena :as mm-arena]
            [mad-max.mm-player :as mm-player]
            [mad-max.indestructible-wall :as ind-wall]
            [mad-max.util :as util]
            [mad-max.arena :as arena]
            [mad-max.actions :as actions]
            [mad-max.renderer :as renderer]
            [mad-max.server :as server]))

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

(defn stop-actions-thread []
  (if (nil? @actions-thread)
    (util/debug-print "No action thread running, can't stop")
    (do
      (.stop @actions-thread)
      (reset! actions-thread nil))))

(defn reset-all-state []
  (dosync
    (ref-set arena-id 0)
    (ref-set arenas {})
    (ref-set entity-id 0)
    (ref-set entities {})
    (ref-set clients {})
    (ref-set special-entity-ids {}))
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
  (alter arenas update-in [a-id :clients] conj client))

(defn remove-entity-from-cell [a-id cell e-id]
  (alter arenas update-in [a-id :entities-map cell] disj e-id)
  (alter entities update e-id dissoc :cell))

(defn remove-entity [e-id]
  (let [{:keys [cell arena-id]} (@entities e-id)]
    (remove-entity-from-cell arena-id cell e-id)
    (alter entities dissoc e-id)))

(defn remove-client [client p-id a-id]
  (alter arenas update-in [a-id :clients] disj client)
  (alter clients dissoc client)
  (remove-entity p-id))

(defn place-entity-at-cell [a-id cell e-id]
  (alter arenas update-in [a-id :entities-map cell] conj e-id)
  (alter entities update e-id assoc :cell cell))

(defn move-from-cell-to-cell [a-id old-cell new-cell e-id]
  (remove-entity-from-cell a-id old-cell e-id)
  (place-entity-at-cell a-id new-cell e-id))

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

(defn remove-player [client]
  (dosync
    (let [{:keys [player-id arena-id]} (@clients client)]
      (remove-client client player-id arena-id))))

(defn render-and-send-arena! [a-id]
  (let [arena (get @arenas a-id)
        render (renderer/render-arena arena @entities)
        clients (seq (get arena :clients))]
    (doseq [client clients]
      (let [{:keys [width height]} (get-in @clients [client :window])]
        (server/send-full-frame client (renderer/resize-screen render width height))))))

(defn upmerge-map-ref [_ref key data]
  (dosync
    (util/debug-print "Updating: " @_ref)
    (util/debug-print "  Key: " key)
    (util/debug-print "  Data: " data)
    (alter _ref update key merge data)))

(declare initialize)

(defn dispatch-action [action]
  (util/debug-print "Recv action: " action)
  (let [action-type (get action :type)]
    (case action-type
      :client-connect (make-and-add-player (action :client))
      :client-disconnect (remove-player (action :client))
      :client-disconnect-all (map remove-player (keys @clients))
      :print (println (action :message))
      :update-client (upmerge-map-ref clients (action :client) (action :data))
      :stop-controller (reset-all-state)
      (util/debug-print "Unknown action: " action))))

(defn process-actions []
  (loop []
    (if (empty? @actions/queue)
      nil
      (do (dispatch-action (actions/dequeue))
          (recur)))))

(defn actions-loop []
  (loop []
    (process-actions)
    (Thread/sleep 10)
    (recur)))

(defn start-actions-thread []
  (if (nil? @actions-thread)
    (reset! actions-thread
      (doto
        (Thread. actions-loop)
        (.setDaemon true)
        (.start)))
    (util/debug-print "Action thread's already running")))

(defn initialize []
  (dosync
    (let [ind-wall (ind-wall/make-indestructible-wall)
          ind-wall-id (add-entity ind-wall nil)]
      (add-special-entity-id ind-wall ind-wall-id)))
  (start-actions-thread))

(defn restart []
  (reset-all-state)
  (initialize))

(defn print-debug-info []
  (util/debug-print "Clients: " @clients)
  (util/debug-print "Arena 0: " (@arenas 0))
  (util/debug-print "Entities: " @entities))
