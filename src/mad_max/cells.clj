(ns mad-max.cells
  (:require [mad-max.util :as util]))

(def dir-to-movement {:up [dec :y]
                      :down [inc :y]
                      :left [dec :x]
                      :right [inc :x]})

(defn change-cell [cell direction]
  (update cell
          (second (dir-to-movement direction))
          (first (dir-to-movement direction))))

(defn remove-cell-with-possible-collision [game arena-id cell]
  (update-in game [:arenas arena-id :cells-with-possible-collisions] disj cell))

(defn place-entity-at-cell-at-arena [game entity-id cell arena-id]
  (-> game
      (assoc-in [:entities entity-id :cell] cell)
      (update-in [:arenas arena-id :entities-map cell] conj entity-id)
      (update-in [:arenas arena-id :cells-with-possible-collisions] conj cell)))

(defn place-entity-at-cell [game entity-id cell]
  (let [arena-id (get-in game [:entities entity-id :arena-id])]
    (place-entity-at-cell-at-arena game entity-id cell arena-id)))

(defn remove-entity-from-cell [game entity-id]
  (let [arena-id (get-in game [:entities entity-id :arena-id])
        cell (get-in game [:entities entity-id :cell])]
    (-> game
        (assoc-in [:entities entity-id :old-cell] cell)
        (assoc-in [:entities entity-id :cell] {:x -1 :y -1})
        (update-in [:arenas arena-id :entities-map cell] disj entity-id))))

(defn real-cell-to-cell [cell]
  (util/mapvals cell #(int (+ 0.5 %))))
