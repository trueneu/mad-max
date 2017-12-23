(ns mad-max.grenade
  (:require [mad-max.util :as util]))
(def velocity 0.05)

(def dir-to-velocity {:up {:y (- velocity)}
                      :down {:y velocity}
                      :left {:x (- velocity)}
                      :right {:x velocity}})

(defn make-grenade [& {:keys [direction player-id real-cell arena-id]}]
  {:direction direction
   :type      :grenade
   :damage    5
   :shred-damage 2
   :passable? true
   :player-id player-id
   :velocity (merge {:x 0 :y 0} (dir-to-velocity direction))
   :destructible? true
   :real-cell real-cell
   :arena-id arena-id
   :time-to-explode 150
   :time-to-drop 80})

(defn real-cell-to-cell [cell]
  (util/mapvals cell #(int (+ 0.5 %))))

(defn representation []
  \*)
