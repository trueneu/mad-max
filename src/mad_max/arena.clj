(ns mad-max.arena)

(defn cell-empty? [arena coords]
  (and (< 0 (:x coords) (dec (:width (:dimensions arena))))
       (< 0 (:y coords) (dec (:height (:dimensions arena))))))

(defn arena-indestructible-walls-cells [arena]
  (let [{:keys [width height]} (get arena :dimensions)]
    (concat []
            (for [i (range width)]
              {:x i :y 0})
            (for [i (range width)]
              {:x i :y (dec height)})
            (for [j (range 1 (dec height))]
              {:x 0 :y j})
            (for [j (range 1 (dec height))]
              {:x (dec width) :y j}))))

(defn make-arena [dimensions ind-wall-id]
  (let [empty-arena {:dimensions                     (merge {:width 30} {:height 20} dimensions)
                     :entities-map                   {}
                     :clients                        #{}
                     :player-ids                     []
                     :cells-with-possible-collisions #{}
                     :last-used-player-color         :black}
        initialized-map-arena (reduce #(assoc-in %1 [:entities-map %2] #{}) empty-arena
                                      (for [i (range (get-in empty-arena [:dimensions :width]))
                                            j (range (get-in empty-arena [:dimensions :height]))]
                                        {:x i :y j}))]
    (reduce #(update-in %1 [:entities-map %2] conj ind-wall-id)
            initialized-map-arena
            (arena-indestructible-walls-cells initialized-map-arena))))

(defn remove-player-id [arena player-id]
  (update arena :player-ids (fn [coll] (vec (remove #(= % player-id) coll)))))

(defn choose-unoccupied-cell [arena]
  (let [entities-map (arena :entities-map)
        unoccupied-cells (map first
                              (filter #(empty? (second %))
                                      entities-map))]
    (rand-nth unoccupied-cells)))