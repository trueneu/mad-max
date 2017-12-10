(ns mad-max.util
  (:require [clojure.pprint :as pp]))

(def DEBUG true)

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