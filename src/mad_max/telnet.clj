(ns mad-max.telnet
  (:require [tenlet.server :as ts]
            [tenlet.escape :as es]
            [mad-max.player :as p]
            [mad-max.bullet :as b]))
(def x (atom 0))
(def y (atom 0))

(def players-colors [:green :yellow
                     :blue :magenta :cyan])

(def wall-color :red)

(def main-color :white)

(def player-starting-char \^)

(def wall-char \*)

(def play-area-w 80)
(def play-area-h 35)

(def key-to-direction {:arrow-down :down
                       :arrow-up :up
                       :arrow-left :left
                       :arrow-right :right})

(def direction-to-char {:up \^
                        :down \v
                        :left \<
                        :right \>})

(defn control-player [connection action]
  (let [move-conn (partial p/move-player connection)]
    (cond
      (contains? key-to-direction action) (p/move-player connection (get key-to-direction action))
      (= action \space) (b/add-bullet (b/make-bullet connection {})))))

(defn colorify [string color]
  (let [color-code (str (es/code color))
        main-color-code (str (es/code main-color))]
    (str color-code string main-color-code)))

(defn draw-at [screen {:keys [x y]} c]
  (assoc-in screen [y x] c))

(defn draw-bullets [screen]
  (reduce #(draw-at %1
                    {:x (get (b/get-bullet-props %2) :drawx)
                     :y (get (b/get-bullet-props %2) :drawy)}
                    (colorify (b/get-bullet-char %2)
                              (get (b/get-bullet-props %2) :color)))
          screen
          (b/get-bullet-ids)))

(defn remove-bullets []
  (doseq [id (b/get-bullet-ids)]
    (let [props (b/get-bullet-props id)]
      (if (false? (get props :alive))
        (b/remove-bullet id)))))

(defn draw-borders [screen]
  (reduce #(draw-at %1 {:x (first %2)
                        :y (second %2)}
                    (colorify wall-char wall-color))
          screen
          (concat
            (for [x (range 1 (inc play-area-w))
                  y [1 play-area-h]]
              [x y])
            (for [x [1 play-area-w]
                  y (range 0 (inc play-area-h))]
              [x y]))))

(defn empty-screen [w h]
  (vec
    (for [j (range h)]
      (vec
        (for [i (range w)]
             " ")))))

(def screen-h 40)
(def screen-w 100)

(def prev-screen (atom (empty-screen screen-w screen-h)))

(def clients (ref #{}))

(defn color []
  (nth players-colors (rand-int (count players-colors))))

(defn screen-to-str [screen]
  (apply str (map #(apply str %) screen)))

;(defn draw-screen [w h]
;  (println (str "x: " @x " y: " @y))
;  (screen-to-str (-> (empty-screen w h) (draw-at {:x @x :y @y} \*))))
;    ;(for [i (range w)
;    ;      j (range h)]
;    ;  "x")))

(defn rewrite-screen [client screen]
  (ts/write client es/CLR)
  (ts/write client (es/cursor 0 0))
  (ts/write client (screen-to-str screen)))


(defn screenw [screen w]
  (let [ow (count (first screen))
        dw (Math/abs (- ow w))]
    (if (not= ow w)
      (if (< ow w)
        (mapv #(into [] (concat % (repeat dw " "))) screen)
        (mapv #(into [] (take dw %)) screen)))))

(defn screenh [screen h]
  (let [ow (count (first screen))
        oh (count screen)
        dh (Math/abs (- oh h))]
    (if (not= oh h)
      (if (< oh h)
        (into [] (concat screen (repeat dh (into [] (repeat ow " ")))))
        (into [] (drop dh screen))))))

(defn resize-screen [screen w h]
  (-> screen (screenw w) (screenh h)))

(defn screen-diff [old-s new-s]
  (for [j (range screen-h)
        i (range screen-w)
        :when (not= (get-in old-s [j i])
                    (get-in new-s [j i]))]
    {:x i :y j :s (get-in new-s [j i])}))

(defn write-diff [connection diff]
  (doseq [d diff]
    (let [{:keys [x y s]} d]
      (ts/write connection (es/cursor x y))
      (ts/write connection s))))

(defn redraw []
  (let [es (empty-screen screen-w screen-h)
        s (reduce #(draw-at %1 %2 \X) es (p/get-players-property :coords))
        connections (p/get-players-connections)]
      (doseq [c connections]
        (let [w (get-in @p/players [c :term :w])
              h (get-in @p/players [c :term :h])]
          (println c)
          (rewrite-screen c (resize-screen s w h))
          (ts/write c (es/cursor 0 (dec h)))))))

(defn draw-players [screen]
  (reduce #(draw-at %1 (first %2)
                       (colorify (nth %2 2)
                                 (second %2)))
          screen
          (p/get-players-properties [:coords :color :char])))

(defn draw-screen []
  (-> (empty-screen screen-w screen-h)
      (draw-borders) (draw-players) (draw-bullets)))

(defn detect-collisions []
  (for [player @p/players
        bullet @b/bullets
        :when (= (get (second player) :coords)
                 (hash-map :x (get (second bullet) :drawx)
                           :y (get (second bullet) :drawy)))]
    [player bullet]))

(defn process-collisions [collisions]
  (doseq [[[connection player-props] [bullet-id bullet-props]] collisions]
    (b/kill-bullet bullet-id)
    (p/take-a-hit connection (get bullet-props :damage))
    (println (str "health: " (get-in @p/players [connection :health])))))

(defn redraw-diff []
  (let [s (draw-screen)
        connections (p/get-players-connections)]
    (let [diff (screen-diff @prev-screen s)]
      (if (not (empty? diff))
        (println diff))
      (reset! prev-screen s)
      (doseq [c connections]
        (let [w (get-in @p/players [c :term :w])
              h (get-in @p/players [c :term :h])]
          (write-diff c diff)
          (ts/write c (es/cursor 0 (dec h))))))))

(defn write-es [connection w h]
  (rewrite-screen connection (resize-screen (empty-screen screen-w screen-h) w h)))

(defn redraw-thread []
  (loop []
    (remove-bullets)
    (b/move-all-bullets)
    (process-collisions (detect-collisions))
    (p/process-deads)
    (redraw-diff)
    (Thread/sleep 25)
    (recur)))

(defn line [client string]
  (println string)
  (println client))

(defn resize [client {:keys [w h]}]
  (println "resized")
  (p/set-term client w h))

  ;(write-es client w h))

(defn connect [client]
  ;(ts/write client (str es/IAC es/DONT es/HIDE))
  (ts/write client (str es/IAC es/DO es/LINE))
  (ts/write client (str es/IAC es/DO es/NAWS))
  (ts/write client (str es/IAC es/WILL es/ECHO))
  (ts/write client es/CLR)
  (reset! prev-screen (empty-screen screen-w screen-h))
  (p/add-player
    (p/make-player client
                   {:coords {:x 5
                             :y 5}
                    :vel {:x 0
                          :y 0}
                    :health 25
                    :term {:w 0
                           :h 0}
                    :color (color)
                    :direction :up
                    :alive true})))



(defn input [client c]
  (println c)
  (control-player client c))

(defn connection-close [client]
  (p/remove-player client))

(defn start-server []
  (ts/create-server 5073
     {:connect  connect
      :line     line
      :input    input
      :resize   resize
      :close    connection-close
      :shutdown (fn [server]
                  (println "Bye!"))}))

(defn create-redraw-thread []
  (Thread. redraw-thread))

(defn start-thread [thread]
  (doto
   thread
   (.setDaemon true)
   (.start)))
