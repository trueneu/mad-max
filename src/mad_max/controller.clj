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

(defn add-entity [entity]
  (alter entities assoc @entity-id entity)
  (alter entity-id inc)
  (dec @entity-id))

(defn add-special-entity-id [entity e-id]
  (alter special-entity-ids assoc (get entity :type) e-id))

(defn get-special-entity-id [entity-type]
  (get @special-entity-ids entity-type))

(defn add-connected-client [client p-id a-id]
  (alter clients assoc client {:player-id p-id :arena-id a-id :window {:width 0 :height 0}})
  (alter arenas update-in [a-id :clients] conj client))

(defn place-entity-at-cell [a-id cell e-id]
  (alter arenas update-in [a-id :entities-map cell] conj e-id))

(defn remove-entity-from-cell [a-id cell e-id]
  (alter arenas update-in [a-id :entities-map cell] disj e-id))

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
          player-id (add-entity player)]
      (util/debug-print "Making and adding player:")
      (util/debug-print "chosen-arena-id: " chosen-arena-id)
      (util/debug-print "player: " player)

      (add-connected-client client player-id chosen-arena-id)
      (place-entity-at-cell chosen-arena-id {:x 1 :y 1} player-id)

      (util/debug-print "resulting arena: " chosen-arena)
      player)))

(defn render-and-send-arena! [a-id]
  (let [arena (get @arenas a-id)
        render (renderer/render-arena arena @entities)
        clients (seq (get arena :clients))]
    (doseq [client clients]
      (let [{:keys [width height]} (get-in @clients [client :window])]
        (server/send-full-frame client (renderer/resize-screen render width height))))))

(defn update-map-ref [_ref key data]
  (dosync
    (alter _ref update key data)))

(declare initialize)

(defn dispatch-action [action]
  (util/debug-print "Recv action: " action)
  (let [action-type (get action :type)]
    (case action-type
      :make-and-add-player (make-and-add-player (get action :client))
      :print (println (get action :message))
      :update-client (update-map-ref clients (get action :client) (get action :data))
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
          ind-wall-id (add-entity ind-wall)]
      (add-special-entity-id ind-wall ind-wall-id)))
  (start-actions-thread))

(defn restart []
  (reset-all-state)
  (initialize))
