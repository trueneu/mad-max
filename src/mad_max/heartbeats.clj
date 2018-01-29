(ns mad-max.heartbeats
  (:require [mad-max.grenade :as mm-grenade]
            [mad-max.mm-bullet :as mm-bullet]
            [mad-max.cells :as cells]
            [mad-max.entities :as entities]
            [mad-max.arena :as arena]))

(defmulti heartbeat (fn [game entity-id] (get-in game [:entities entity-id :type])))

;(defmethod heartbeat :indestructible-wall [game _])
;(defmethod heartbeat :holy-grenade [_])
;(defmethod heartbeat :health-powerup [_])

(defmethod heartbeat :default [game _]
  game)

(defmethod heartbeat :grenade [game grenade-id]
  (let [grenade (get-in game [:entities grenade-id])
        {:keys [real-cell velocity arena-id cell]} grenade
        new-real-cell (merge-with + real-cell velocity)
        new-cell (cells/real-cell-to-cell new-real-cell)]
    (-> game
      (update-in [:entities grenade-id] merge {:real-cell new-real-cell})
      (update-in [:entities grenade-id :time-to-explode] dec)
      (update-in [:entities grenade-id :time-to-drop] dec)
      (cells/remove-entity-from-cell grenade-id)
      ((fn [g] (if (pos? (get-in g [:entities grenade-id :time-to-drop]))
                   g
                   (entities/stop-entity g grenade-id))))
      ((fn [g] (if (pos? (get-in g [:entities grenade-id :time-to-explode]))
                   (do (-> g
                           (cells/place-entity-at-cell grenade-id new-cell)))
                   (do (-> g
                           (mm-grenade/explode-grenade grenade-id)
                           (entities/remove-entity grenade-id)))))))))

(defmethod heartbeat :player [game player-id]
  (let [player (get-in game [:entities player-id])]
    (if-not (player :alive?)
      (if (pos? (player :time-to-vanish))
        (update-in game [:entities player-id :time-to-vanish] dec)
        (-> game
            (cells/remove-entity-from-cell player-id)
            (entities/remove-entity player-id)
            (update-in [:arenas (player :arena-id)] arena/remove-player-id player-id)))
      game)))


(defmethod heartbeat :bullet [game bullet-id]
  (let [bullet (get-in game [:entities bullet-id])
        {:keys [real-cell velocity arena-id cell]} bullet
        new-real-cell (merge-with + real-cell velocity)
        new-cell (cells/real-cell-to-cell new-real-cell)]
    (-> game
        (update-in [:entities bullet-id] merge {:real-cell new-real-cell})
        (cells/remove-entity-from-cell bullet-id)
        (cells/place-entity-at-cell bullet-id new-cell))))

;(defn arena-heartbeat [a-id]
;  (let [r (rand-int health-powerup-chance)]
;    (when (zero? r)
;      (dosync
;        (let [entities-map ((@arenas a-id) :entities-map)
;              empty-cells (filter #(empty? (second %)) entities-map)]
;          (when-not (empty? empty-cells)
;            (let [empty-cell (first (rand-nth empty-cells))]
;              (make-and-add-health-powerup a-id empty-cell))))))))
