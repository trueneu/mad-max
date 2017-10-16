(ns mad-max.bullet
  (:require [mad-max.player :as p]))
(def bullets (ref {}))
(def bullet-id (atom 0))

(def bullet-speed 0.2)

(defn make-bullet [owner props]
  (let [{:keys [x y]} (get-in @p/players [owner :coords])
        [bx by bvelx bvely] (case (get-in @p/players [owner :direction])
                              :up [x (dec y) 0 (- bullet-speed)]
                              :down [x (inc y) 0 bullet-speed]
                              :left [(dec x) y (- bullet-speed) 0]
                              :right [(inc x) y bullet-speed 0])
        color (get-in @p/players [owner :color])]
    (merge
      {:owner owner
       :x bx
       :y by
       :drawx bx
       :drawy by
       :velx bvelx
       :vely bvely
       :color color
       :alive true
       :damage 5}
      props)))

(defn add-bullet [props]
  (dosync
    (alter bullets assoc @bullet-id props)
    (swap! bullet-id inc)))

(defn move-bullet [id]
  (dosync
    (let [{:keys [x y velx vely]} (get @bullets id)
          newx (+ x velx)
          newy (+ y vely)
          newdrawx (int (+ newx 0.5))
          newdrawy (int (+ newy 0.5))
          newalive (p/check-coords {:x newdrawx
                                    :y newdrawy})]
      (alter bullets
             update id
             #(merge % {:x newx
                        :y newy
                        :drawx newdrawx
                        :drawy newdrawy
                        :alive newalive})))))

(defn kill-bullet [id]
  (dosync
    (alter bullets
           update id
           #(assoc % :alive false))))

(defn get-bullet-ids []
  (keys @bullets))

(defn move-all-bullets []
  (doseq [id (get-bullet-ids)] (move-bullet id)))

(defn get-bullet-char [id]
  (case (get-in @bullets [id :velx])
    0 \|
    \-))

(defn get-bullet-props [id]
  (get @bullets id))

(defn remove-bullet [id]
  (dosync
    (alter bullets dissoc id)))