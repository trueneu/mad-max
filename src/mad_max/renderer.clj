(ns mad-max.renderer
  (:require [mad-max.arena :as mm-arena]
            [mad-max.mm-player :as mm-player]
            [mad-max.indestructible-wall :as ind-wall]
            [mad-max.mm-bullet :as mm-bullet]
            [mad-max.util :as util]
            [mad-max.health-powerup :as mm-health-powerup]
            [mad-max.grenade :as mm-grenade]))

(def blank-lines-health-stats 2)
(def blank-space-health-stats 2)

(def min-width 80)
(def min-height 30)

(defmulti representation (fn [entity] (get entity :type nil)))

(defmethod representation :player [entity]
  (mm-player/representation entity))

(defmethod representation :bullet [entity]
  (mm-bullet/representation entity))

(defmethod representation :grenade [entity]
  (mm-grenade/representation))

(defmethod representation :indestructible-wall [_]
  (ind-wall/representation))

(defmethod representation :health-powerup [_]
  (mm-health-powerup/representation))

(defmethod representation nil [_]
  \space)

(defn empty-screen [w h]
  (try
    (vec (for [j (range h)]
           (vec (for [i (range w)]
                     " "))))
    (catch Exception e (println "Exception " e " w: " w " h: " h))))

(defn draw-at [screen {:keys [x y]} char]
  (assoc-in screen [y x] char))

(defn render-map [arena all-entities]
  (let [{:keys [width height]} (get arena :dimensions)
        es (empty-screen width height)
        entities-map (get arena :entities-map)]
    (loop [screen es
           cells (keys entities-map)]
      (if (empty? cells)
        screen
        (let [cell (first cells)
              entity-ids (get entities-map cell)
              entities (map #(get all-entities %) entity-ids)]
          (recur
            (reduce #(draw-at %1 cell %2) screen (map representation entities))
            (rest cells)))))))

(defn pretty-print-screen [screen]
  (print
    (loop [string ""
           s screen]
      (if (empty? s)
        string
        (let [line (first s)]
          (recur (str string (apply str line) ";" "\n")
                 (rest s)))))))

(defn resize-screen-width [screen desired-width & {:keys [fill-char] :or {fill-char \space}}]
  (let [original-width (count (first screen))
        diff-width (Math/abs ^Integer (- original-width desired-width))]
    (if (not= original-width desired-width)
      (if (< original-width desired-width)
        (mapv #(into [] (concat % (repeat diff-width fill-char))) screen) ; FIXME mapv into []?
        (mapv #(into [] (take diff-width %)) screen)))))

(defn resize-screen-height [screen desired-height & {:keys [fill-char] :or {fill-char \space}}]
  (let [original-width (count (first screen))
        original-height (count screen)
        diff-height (Math/abs ^Integer (- original-height desired-height))]
    (if (not= original-height desired-height)
      (if (< original-height desired-height)
        (into [] (concat screen (repeat diff-height (into [] (repeat original-width fill-char)))))
        (into [] (drop diff-height screen))))))

(defn resize-screen [screen desired-width desired-height]
  (-> screen (resize-screen-width desired-width) (resize-screen-height (dec desired-height))))

(defn screen-to-full-frame [screen]
  (apply str (map (partial apply str) screen)))

(defn place-string-at [screen coords s]
  (let [{:keys [x y]} coords
        width  (count (screen 0))
        height (count screen)
        string-width (count s)
        string-vector (vec s)]
    (if (or (> y (dec height)) (> x (- (dec width) string-width)))
      (do
        (util/debug-print (str "Couldn't output string " s " at: " coords))
        screen)
      (reduce (fn [scr-acc idx]
                (draw-at scr-acc {:x (+ x idx) :y y} (string-vector idx)))
              screen
              (range string-width)))))

(defn form-health-string [player]
  (str (player :name) "'s HP: "
       (if (> (player :health) 0)
         (player :health)
         "as dead as Jim Morisson")))

(defn add-health-stats [render arena all-entities]
  (dosync
    (let [player-ids (arena :player-ids)
          players-count (count (arena :player-ids))
          ;first-health-stats-y (dec (+ blank-lines-health-stats (get-in arena [:dimensions :height])))
          first-health-stats-y 0
          health-stats-x (dec (+ blank-space-health-stats (get-in arena [:dimensions :width])))]
      (reduce (fn [scr-acc idx] (place-string-at scr-acc {:x health-stats-x :y (+ first-health-stats-y idx)}
                                                 (form-health-string (all-entities (player-ids idx)))))
              render
              (range players-count)))))

(defn render-arena [arena all-entities]
  (let [render (render-map arena all-entities)]
    (-> render (resize-screen-height min-height) (resize-screen-width min-width)
      (add-health-stats arena all-entities))))
