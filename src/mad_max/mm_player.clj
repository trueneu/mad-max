(ns mad-max.mm-player
  (:require [mad-max.coords :as coords]
            [mad-max.arena :as arena]))

(def dir-to-player-representation {:up    \^
                                   :down  \v
                                   :left  \<
                                   :right \>})

(def dead-representation \`)

(defn make-player [& {:keys [health color direction]
                      :or   {health    10
                             color     :black
                             direction :up}}]
  {:health    health
   :color     color
   :direction direction
   :type      :player
   :passable? true
   :destructible? true
   :alive? true
   :time-to-vanish 500})

(defn move [player direction]
  (let [arena (:arena player)
        new-coords (coords/change (:coords player) direction)
        new-cell-empty (arena/cell-empty? (:arena player) new-coords)
        new-properties (concat [:direction direction]
                               (if new-cell-empty [:coords new-coords] []))]

    (apply assoc player new-properties)))

(defn representation [player]
  (if (player :alive?)
    (get dir-to-player-representation (:direction player))
    dead-representation))
