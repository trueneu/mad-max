(ns mad-max.util)

(def DEBUG true)

(defn debug-print [& more]
  (if DEBUG
    (apply println more)))
