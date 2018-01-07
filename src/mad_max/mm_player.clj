(ns mad-max.mm-player)

(def dir-to-player-representation {:up    \^
                                   :down  \v
                                   :left  \<
                                   :right \>})

(def dead-representation \`)

(defn make-player [name arena-id & {:keys [health color direction]
                                    :or   {health    10
                                           color     :black
                                           direction :up}}]
  {:health    health
   :color     color
   :direction direction
   :type      :player
   :passable? true
   :destructible? true
   :alive? true
   :time-to-vanish 500
   :grenades 3
   :arena-id arena-id
   :name name})

(defn representation [player]
  (if (player :alive?)
    (get dir-to-player-representation (:direction player))
    dead-representation))

(defn take-a-hit [])

(defn take-a-hit [player damage]
  (let [new-health (- (player :health) damage)]
    (->
      player
      (assoc :health new-health)
      (merge (if (<= new-health 0) {:destructible? false :alive? false})))))
