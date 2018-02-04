(ns mad-max.cells
  (:require [mad-max.util :as util]))

(def dir-to-movement {:up         [[dec :y]]
                      :down       [[inc :y]]
                      :left       [[dec :x]]
                      :right      [[inc :x]]
                      :up-right   [[inc :x] [dec :y]]
                      :down-left  [[inc :y] [dec :x]]
                      :up-left    [[dec :x] [dec :y]]
                      :down-right [[inc :y] [inc :x]]})

(def dir-to-opposite {:up         :down
                      :down       :up
                      :left       :right
                      :right      :left
                      :up-right   :down-left
                      :down-left  :up-right
                      :up-left    :down-right
                      :down-right :up-left})

(defn change-cell [cell direction]
  (reduce (fn [acc [f x]]
            (update acc x f))
          cell
          (dir-to-movement direction)))

(defn remove-cell-with-possible-collision [game arena-id cell]
  (update-in game [:arenas arena-id :cells-with-possible-collisions] disj cell))

(defn place-entity-at-cell-at-arena [game entity-id cell arena-id]
  ;(util/debug-print "placing " entity-id)
  ;(util/debug-print "  at cell " cell)
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
    (when (nil? cell)
      (util/debug-print "got nil cell when removing!")
      (util/debug-print "entity id: " entity-id)
      (util/debug-print "entity: " (get-in game [:entities entity-id])))
    (-> game
        (assoc-in [:entities entity-id :old-cell] cell)
        (assoc-in [:entities entity-id :cell] {:x -1 :y -1})
        (update-in [:arenas arena-id :entities-map cell] disj entity-id))))

(defn real-cell-to-cell [cell]
  (util/mapvals cell #(int (+ 0.5 %))))

(defn cell-valid? [cell dimensions]
  (let [{:keys [width height]} dimensions
        {:keys [x y]} cell]
    (and (<= 1 x (- width 2))
         (<= 1 y (- height 2)))))
