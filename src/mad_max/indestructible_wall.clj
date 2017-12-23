(ns mad-max.indestructible-wall)

(defn make-indestructible-wall []
  {:type :indestructible-wall
   :passable? false
   :destructible? false})

(defn representation []
  \X)