(ns mad-max.util
  (:require [clojure.pprint :as pp]))

(def DEBUG true)

(def directions [:up :down :left :right])

(defn debug-print
  ([desc]
   (when DEBUG
     (println desc)))
  ([desc obj]
   (when DEBUG
     (println desc)
     (pp/pprint obj))))

(defn mapvals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn debug-game-wo-renders [game]
  (let [arena-ids (keys (game :arenas))
        game-wo-renders (reduce #(update-in %1 [:arenas %2] dissoc :render)
                                game
                                arena-ids)]
    (pp/pprint game-wo-renders)))