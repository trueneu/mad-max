(ns mad-max.health-powerup)

(def health-powerup-inverse-probability 5000)

(defn health-powerup-chance? []
  (zero? (rand-int health-powerup-inverse-probability)))

(defn make-health-powerup [arena-id]
  {:type      :health-powerup
   :passable? true
   :destructible? false
   :arena-id arena-id
   :heal 3})

(defn representation []
  \+)
