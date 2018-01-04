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
            [mad-max.game :as mm-game]))

(defn add-entity [game entity]
  (-> game
    (assoc-in [:entities (game :entity-id)] entity)
    (update :entity-id inc)))

(defn add-special-entity [game entity]
  (-> game
      (add-entity entity)
      (assoc-in [:special-entities-type-to-id (entity :type)] (game :entity-id))))

(defn init-game [game]
  (let [ind-wall (ind-wall/make-indestructible-wall)
        h-grenade (holy-grenade/make-holy-grenade)]
    (reduce add-special-entity game [ind-wall h-grenade])))

(defn main-loop []
  (loop [game (-> mm-game/make-game init-game)]))
  ; process current game
  ; process events
  ; send frames to clients
