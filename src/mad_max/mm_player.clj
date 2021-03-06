(ns mad-max.mm-player)

(def dir-to-player-representation {:up    \^
                                   :down  \v
                                   :left  \<
                                   :right \>})

(def dead-representation \`)

(def great-dead ["Jim Morrisson" "Kurt Cobain" "Freddy Mercury" "Jimi Hendrix"
                 "Amy Winehouse" "Brian Jones" "Janis Joplin" "Chuck Berry"
                 "Barry White"])

(defn make-player [name arena-id client-connection
                   & {:keys [health color direction]
                      :or   {health    10
                             color     :black
                             direction :up}}]
  {:health            health
   :color             color
   :direction         direction
   :type              :player
   :passable?         true
   :destructible?     true
   :alive?            true
   :time-to-vanish    500
   :grenades          3
   :arena-id          arena-id
   :name              name
   :client-connection client-connection
   :dead-name         (rand-nth great-dead)})

(defn representation [player]
  (if (player :alive?)
    (get dir-to-player-representation (:direction player))
    dead-representation))

(defn take-a-hit [player damage]
  (let [new-health (- (player :health) damage)]
    (->
      player
      (assoc :health new-health)
      (merge (if (<= new-health 0) {:destructible? false :alive? false})))))

(defn heal [player hp]
  (update player :health + hp))

(defn insta-death [player]
  (take-a-hit player (player :health)))