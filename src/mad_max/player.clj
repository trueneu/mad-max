(ns mad-max.player)

(def players (ref {}))

(def play-area-w 80)
(def play-area-h 35)
(def dead-char \`)

(defn make-player [connection properties]
  [connection properties])

(defn add-player [player]
  (dosync
    (alter players assoc (first player) (second player))))

(defn get-players-property [prop]
  (map #(prop %) (vals @players)))

(defn get-players-properties [props]
  (for [player (vals @players)]
    (map #(% player) props)))

(defn get-players-connections []
  (keys @players))

(defn set-term [connection w h]
  (dosync
    (alter players assoc-in [connection :term] {:w w :h h})))

(defn alter-player [kw f]
  (dosync
    (alter players assoc-in kw (f (get-in @players kw)))))

(defn check-coords [coords]
  (and (<= 2 (:x coords) (dec play-area-w))
       (<= 2 (:y coords) (dec play-area-h))))

(defn alter-player-coords [connection f]
  (dosync
    (let [new-coords (f (get-in @players [connection :coords]))]
      (println new-coords)
      (if (check-coords new-coords)
        (alter players assoc-in [connection :coords] new-coords)))))

(defn move-player [connection direction]
  (if (true? (get-in @players [connection :alive]))
    (dosync
      (let [[f new-char] (case direction
                          :up [#(update % :y dec) \^]
                          :down [#(update % :y inc) \v]
                          :left [#(update % :x dec) \<]
                          :right [#(update % :x inc) \>])
            new-coords (f (get-in @players [connection :coords]))]
        (println new-coords)
        (alter players assoc-in [connection :char] new-char)
        (alter players assoc-in [connection :direction] direction)
        (if (check-coords new-coords)
          (alter players assoc-in [connection :coords] new-coords))))))

(defn reset-players []
  (dosync
    (ref-set players {})))

(defn remove-player [connection]
  (dosync
    (alter players dissoc connection)))

(defn process-deads []
  (doseq [connection (get-players-connections)]
    (if (false? (get-in @players [connection :alive]))
      (dosync
        (alter players assoc-in [connection :char] dead-char)))))

(defn take-a-hit [connection damage]
  (dosync
    (alter players update connection #(-> % (update :health
                                              (fn [h] (- h damage)))
                                          (assoc :alive
                                                 (> (- (get % :health) damage) 0))))))
