(ns mad-max.mm-bullet
  (:require [mad-max.util :as util]
            [mad-max.cells :as cells]
            [mad-max.entities :as entities]))

(def dir-to-representation {:up         \|
                            :down       \|
                            :left       \-
                            :right      \-
                            :up-right   \/
                            :down-left  \/
                            :up-left    \\
                            :down-right \\})

(def velocity 0.3)

(def dir-to-velocity {:up         {:y (- velocity)}
                      :down       {:y velocity}
                      :left       {:x (- velocity)}
                      :right      {:x velocity}
                      :up-right   {:y (- velocity) :x velocity}
                      :up-left    {:y (- velocity) :x (- velocity)}
                      :down-left  {:y velocity :x (- velocity)}
                      :down-right {:y velocity :x velocity}})

(defn make-bullet [arena-id & {:keys [direction player-id real-cell color damage]
                               :or   {:color  :white
                                      :damage 1}}]
  {:direction     direction
   :type          :bullet
   :damage        damage
   :passable?     true
   :player-id     player-id
   :velocity      (merge {:x 0 :y 0} (dir-to-velocity direction))
   :destructible? true
   :real-cell     real-cell
   :arena-id      arena-id
   :cell          (cells/real-cell-to-cell real-cell)
   :color         color})

(defn representation [bullet]
  (get dir-to-representation (:direction bullet)))
