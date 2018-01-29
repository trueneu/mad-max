(ns mad-max.grenade
  (:require [mad-max.util :as util])
  (:require [mad-max.cells :as cells]
            [mad-max.mm-bullet :as mm-bullet]
            [mad-max.entities :as entities]))

(def velocity 0.15)

(def dir-to-velocity {:up {:y (- velocity)}
                      :down {:y velocity}
                      :left {:x (- velocity)}
                      :right {:x velocity}})

(defn make-grenade [arena-id & {:keys [direction player-id real-cell color]
                                :or {:color :white}}]
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
   :time-to-explode 90
   :time-to-drop 60
   :cell (cells/real-cell-to-cell real-cell)
   :color color})

(defn representation []
  \*)

(defn explode-grenade [game grenade-id]
  (let [{:keys [direction real-cell arena-id player-id color]} (get-in game [:entities grenade-id])
        cell (cells/real-cell-to-cell real-cell)
        dimensions (get-in game [:arenas arena-id :dimensions])]
    (reduce
      (fn [g bullet-direction]
        (let [bullet-cell (cells/change-cell cell bullet-direction)
              bullet-cell-corrected (if (cells/cell-valid? bullet-cell dimensions)
                                      bullet-cell
                                      (cells/change-cell bullet-cell (cells/dir-to-opposite direction)))]
          (if (cells/cell-valid? bullet-cell-corrected dimensions)
            (let [bullet (mm-bullet/make-bullet arena-id :direction bullet-direction :player-id player-id :real-cell bullet-cell-corrected :color color)
                  bullet-id (g :entity-id)]
              (-> g
                (entities/add-entity bullet)
                (cells/place-entity-at-cell bullet-id bullet-cell-corrected)))
            g)))
      game
      [:up :down :left :right])))
