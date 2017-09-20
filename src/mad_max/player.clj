(ns mad-max.player)

(def players (ref {}))

(defn make-player [connection properties]
  [connection properties])

(defn add-player [player]
  (dosync
    (alter players assoc (first player) (second player))))

(defn get-players-property [prop]
  (map #(prop %) (vals @players)))

(defn get-players-connections []
  (keys @players))

(defn set-term [connection w h]
  (dosync
    (alter players assoc-in [connection :term] {:w w :h h})))

(defn alter-player [kw f]
  (dosync
    (alter players assoc-in kw (f (get-in @players kw)))))

(defn reset-players []
  (dosync
    (ref-set players {})))

(defn remove-player [connection]
  (dosync
    (alter players dissoc connection)))