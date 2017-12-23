(ns mad-max.health-powerup)

(defn make-health-powerup [& {:keys [arena-id]}]
  {:type      :health-powerup
   :passable? true
   :destructible? false
   :arena-id arena-id
   :heal 3})

(defn representation []
  \+)
