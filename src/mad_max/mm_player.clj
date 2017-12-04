(ns mad-max.mm-player
  (:require [mad-max.coords :as coords]
            [mad-max.arena :as arena]))

(def dir-to-player-representation {:up \^
                                   :down \v
                                   :left \<
                                   :right \>})

(defn make-player [& {:keys [health color direction]
                      :or   {health    10
                             color     :black
                             direction :up}}]
  {:health health
   :color  color
   :dir    direction
   :type   :player})

(defn alive? [player]
  (> (:health player) 0))

(defn move [player direction]
  (let [arena (:arena player)
        new-coords (coords/change (:coords player) direction)
        new-cell-empty (arena/cell-empty? (:arena player) new-coords)
        new-properties (concat [:direction direction]
                               (if new-cell-empty [:coords new-coords] []))]

    (apply assoc player new-properties)))

(defn representation [player]
  (get dir-to-player-representation (:dir player)))
