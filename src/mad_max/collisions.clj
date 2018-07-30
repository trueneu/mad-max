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

(defmethod collision [:grenade :player] [game grenade-id player-id]
  (let [player (get-in game [:entities player-id])
        grenade (get-in game [:entities grenade-id])]
    (if (player :destructible?)
      (->
        game
        (update-in [:entities player-id] mad-max.mm-player/take-a-hit (grenade :damage))
        (mm-grenade/explode-grenade grenade-id)
        (cells/remove-entity-from-cell grenade-id)
        (entities/remove-entity grenade-id))
      game)))

(defmethod collision [:player :grenade] [game player-id grenade-id]
  (collision game grenade-id player-id))

(defmethod collision [:health-powerup :player] [game health-powerup-id player-id]
  (let [player (get-in game [:entities player-id])
        health-powerup (get-in game [:entities health-powerup-id])]
    (if (player :alive?)
      (->
        game
        (update-in [:entities player-id] mad-max.mm-player/heal (health-powerup :heal))
        (cells/remove-entity-from-cell health-powerup-id)
        (entities/remove-entity health-powerup-id))
      game)))

(defmethod collision [:player :health-powerup] [game player-id health-powerup-id]
  (collision game health-powerup-id player-id))

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
        (let [[entity-1 entity-2] (map (g :entities) [entity-id1 entity-id2])]
          ;(util/debug-print "  ent1: " entity-1)
          ;(util/debug-print "  ent2: " entity-2)
          (if (or (nil? entity-1) (nil? entity-2))
            g
            (collision g entity-id1 entity-id2))))
      game
      collision-map)))

(defn detect-and-process [game arena-id]
  (reduce
    (fn [g cell]
      (->
        (if (collision? g arena-id cell)
          (process-collision g arena-id cell)
          g)
        (cells/remove-cell-with-possible-collision arena-id cell)))
    game
    (seq (get-in game [:arenas arena-id :cells-with-possible-collisions]))))
