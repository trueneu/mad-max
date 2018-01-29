(ns mad-max.collisions
  (:require [mad-max.util :as util]
            [mad-max.cells :as cells]
            [mad-max.entities :as entities]
            [clojure.math.combinatorics :as combo]
            [mad-max.mm-player :as mm-player]
            [mad-max.grenade :as mm-grenade]))

(defmulti collision (fn [game entity-id-1 entity-id-2]
                      (let [[entity-1 entity-2] (map (game :entities) [entity-id-1 entity-id-2])]
                        (vector (entity-1 :type) (entity-2 :type)))))

(defmethod collision [:bullet :indestructible-wall] [game bullet-id ind-wall-id]
  (-> game
      (cells/remove-entity-from-cell bullet-id)
      (entities/remove-entity bullet-id)))

(defmethod collision [:indestructible-wall :bullet] [game ind-wall-id bullet-id]
  (collision game bullet-id ind-wall-id))

(defmethod collision [:bullet :player] [game bullet-id player-id]
  (let [player (get-in game [:entities player-id])
        bullet (get-in game [:entities bullet-id])]
    (if (player :destructible?)
      (->
        game
        (update-in [:entities player-id] mad-max.mm-player/take-a-hit (bullet :damage))
        (cells/remove-entity-from-cell bullet-id)
        (entities/remove-entity bullet-id))
      game)))

(defmethod collision [:player :bullet] [game player-id bullet-id]
  (collision game bullet-id player-id))

;(defmethod collision [:health-powerup :bullet] [health-powerup-id bullet-id])
;
;(defmethod collision [:bullet :health-powerup] [bullet-id health-powerup-id]
;  (collision health-powerup-id bullet-id))
;
;(defmethod collision [:health-powerup :player] [health-powerup-id player-id]
;  (heal player-id health-powerup-id))
;
;(defmethod collision [:player :health-powerup ] [player-id health-powerup-id]
;  (collision health-powerup-id player-id))
;
;(defmethod collision [:bullet :bullet] [bullet-id-1 bullet-id-2])
;
;(defmethod collision [:player :player] [player-id-1 player-id-2])
;
;(defmethod collision [:grenade :player] [grenade-id player-id]
;  (when ((@entities player-id) :destructible?)
;    (take-a-hit player-id grenade-id)
;    (explode-grenade grenade-id)
;    (remove-entity grenade-id)))
;
;(defmethod collision [:player :grenade] [player-id grenade-id]
;  (collision grenade-id player-id))
;
;(defmethod collision [:grenade :grenade] [grenade-id-1 grenade-id-2])
;(defmethod collision [:grenade :bullet] [grenade-id bullet-id])
;(defmethod collision [:bullet :grenade] [bullet-id grenade-id])
;(defmethod collision [:grenade :health-powerup] [grenade-id health-powerup-id])
;(defmethod collision [:health-powerup :grenade] [health-powerup-id grenade-id])
(defmethod collision [:grenade :indestructible-wall] [game grenade-id indestructible-wall-id]
  (-> game
    (mm-grenade/explode-grenade grenade-id)
    (cells/remove-entity-from-cell grenade-id)
    (entities/remove-entity grenade-id)))

(defmethod collision [:indestructible-wall :grenade] [game indestructible-wall-id grenade-id]
  (collision game grenade-id indestructible-wall-id))

(defmethod collision :default [game _ _]
  game)

(defn collision? [game arena-id cell]
  (> (count (get-in game [:arenas arena-id :entities-map cell])) 1))

(defn process-collision [game arena-id cell]
  (let [entitiy-ids-at-cell (get-in game [:arenas arena-id :entities-map cell])
        collision-map (combo/combinations entitiy-ids-at-cell 2)]
    (reduce
      (fn [g [entity-id1 entity-id2]]
        (collision g entity-id1 entity-id2))
      game
      collision-map)))

(defn detect-and-process [game arena-id]
  (reduce
    (fn [g cell]
      (->
        (if (collision? g arena-id cell)
          (do
            (process-collision g arena-id cell))
          g)
        (cells/remove-cell-with-possible-collision arena-id cell)))
    game
    (seq (get-in game [:arenas arena-id :cells-with-possible-collisions]))))
