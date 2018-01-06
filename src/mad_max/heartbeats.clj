(ns mad-max.heartbeats
  (:require [mad-max.grenade :as mm-grenade]))

(defmulti heartbeat (fn [entity-id] (get-in @entities [entity-id :type])))

(defmethod heartbeat :indestructible-wall [_])
(defmethod heartbeat :holy-grenade [_])
(defmethod heartbeat :health-powerup [_])

(defmethod heartbeat :grenade [grenade-id]
  (let [grenade (@entities grenade-id)
        {:keys [real-cell velocity arena-id cell]} grenade
        new-real-cell (merge-with + real-cell velocity)
        new-cell (mm-grenade/real-cell-to-cell new-real-cell)]
    (dosync
      (alter entities update grenade-id merge {:real-cell new-real-cell})
      (alter entities update-in [grenade-id :time-to-explode] dec)
      (alter entities update-in [grenade-id :time-to-drop] dec)
      (when (<= ((@entities grenade-id) :time-to-drop) 0)
        (entity-stop grenade-id))
      (if (<= ((@entities grenade-id) :time-to-explode) 0)
        (do
          (explode-grenade grenade-id)
          (remove-entity grenade-id))
        (move-from-cell-to-cell arena-id cell new-cell grenade-id)))))

(defmethod heartbeat :player [p-id]
  (when (not ((player-by-id p-id) :alive?))
    (dosync
      (if (> ((player-by-id p-id) :time-to-vanish) 0)
        (alter entities update-in [p-id :time-to-vanish] dec)
        (remove-player p-id)))))

(defmethod heartbeat :bullet [bullet-id]
  (let [bullet (@entities bullet-id)
        {:keys [real-cell velocity arena-id cell]} bullet
        new-real-cell (merge-with + real-cell velocity)
        new-cell (mm-bullet/real-cell-to-cell new-real-cell)]
    (dosync
      (alter entities update bullet-id merge {:real-cell new-real-cell})
      (move-from-cell-to-cell arena-id cell new-cell bullet-id))))

(defn arena-heartbeat [a-id]
  (let [r (rand-int health-powerup-chance)]
    (when (zero? r)
      (dosync
        (let [entities-map ((@arenas a-id) :entities-map)
              empty-cells (filter #(empty? (second %)) entities-map)]
          (when-not (empty? empty-cells)
            (let [empty-cell (first (rand-nth empty-cells))]
              (make-and-add-health-powerup a-id empty-cell))))))))
