(ns mad-max.renderer
  (:require [mad-max.arena :as mm-arena]
            [mad-max.mm-player :as mm-player]
            [mad-max.indestructible-wall :as ind-wall]))

(defmulti representation (fn [entity] (get entity :type nil)))

(defmethod representation :player [entity]
  (mm-player/representation entity))

(defmethod representation :indestructible-wall [_]
  (ind-wall/representation))

(defmethod representation nil [_]
  " ")

(defn empty-screen [w h]
  (vec (for [j (range h)]
         (vec (for [i (range w)]
                   " ")))))

(defn draw-at [screen {:keys [x y]} char]
  (assoc-in screen [y x] char))

(defn render-arena [arena all-entities]
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

(defn resize-screen-width [screen desired-width & {:keys [fill-char] :or {fill-char " "}}]
  (let [original-width (count (first screen))
        diff-width (Math/abs ^Integer (- original-width desired-width))]
    (if (not= original-width desired-width)
      (if (< original-width desired-width)
        (mapv #(into [] (concat % (repeat diff-width fill-char))) screen) ; FIXME mapv into []?
        (mapv #(into [] (take diff-width %)) screen)))))

(defn resize-screen-height [screen desired-height & {:keys [fill-char] :or {fill-char " "}}]
  (let [original-width (count (first screen))
        original-height (count screen)
        diff-height (Math/abs ^Integer (- original-height desired-height))]
    (if (not= original-height desired-height)
      (if (< original-height desired-height)
        (into [] (concat screen (repeat diff-height (into [] (repeat original-width fill-char)))))
        (into [] (drop diff-height screen))))))

(defn resize-screen [screen desired-width desired-height]
  (-> screen (resize-screen-width desired-width) (resize-screen-height desired-height)))
