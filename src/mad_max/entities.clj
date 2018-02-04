(ns mad-max.entities
  (:require [mad-max.util :as util]))

(defn add-entity [game entity]
  (-> game
      (assoc-in [:entities (game :entity-id)] entity)
      (update :entity-id inc)))

(defn add-special-entity [game entity]
  (-> game
      (add-entity entity)
      (assoc-in [:special-entities-type-to-id (entity :type)] (game :entity-id))))

(defn special-entity-id [game special-entity-type]
  (get-in game [:special-entities-type-to-id special-entity-type]))

(defn remove-entity [game entity-id]
  (->
    game
    (update :entities dissoc entity-id)))

(defn stop-entity [game entity-id]
  (->
    game
    (update-in [:entities entity-id :velocity] merge {:x 0 :y 0})))