(ns mad-max.bullet)

(def bullets (ref {}))
(def bullet-id (atom 0))

(defn add-bullet [props]
  (dosync
    (alter bullets assoc @bullet-id props)
    (swap! bullet-id inc)))

(defn move-bullet [id]
  (dosync
    (let [{:keys [x y velx vely]} (get @bullets id)]
      (alter bullets
             update id
             #(-> % (update :x (fn [x] (+ x velx)))
                  (update :y (fn [y] (+ y vely))))))))

(defn get-bullet-ids []
  (keys @bullets))

(defn move-all-bullets []
  (doseq [id (get-bullet-ids)] (move-bullet id)))

(defn get-bullet-char [id]
  (case (get @bullets :velx)
    0 \|
    \-))

(defn get-bullet-props [id]
  (get @bullets id))

(defn remove-bullet [id]
  (dosync
    (alter bullets dissoc id)))