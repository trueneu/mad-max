(ns mad-max.mm-bullet
  (:require [mad-max.util :as util]))

(def dir-to-representation {:up    \|
                            :down  \|
                            :left  \-
                            :right \-})

(def velocity 0.15)

(def dir-to-velocity {:up {:y (- velocity)}
                      :down {:y velocity}
                      :left {:x (- velocity)}
                      :right {:x velocity}})

(defn make-bullet [& {:keys [direction player-id real-cell arena-id]}]
  {:direction direction
   :type      :bullet
   :damage    1
   :passable? true
   :player-id player-id
   :velocity (merge {:x 0 :y 0} (dir-to-velocity direction))
   :destructible? true
   :real-cell real-cell
   :arena-id arena-id})

(defn real-cell-to-cell [cell]
  (util/mapvals cell #(int (+ 0.5 %))))

(defn representation [bullet]
  (get dir-to-representation (:direction bullet)))