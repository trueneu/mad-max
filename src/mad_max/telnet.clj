(ns mad-max.telnet
  (:require [tenlet.server :as ts]
            [tenlet.escape :as es]
            [mad-max.player :as p]))

(def x (atom 0))
(def y (atom 0))

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

(defn draw-at [screen {:keys [x y]} c]
  (assoc-in screen [y x] c))

(defn screen-to-str [screen]
  (apply str (map #(apply str %) screen)))

(defn draw-screen [w h]
  (println (str "x: " @x " y: " @y))
  (screen-to-str (-> (empty-screen w h) (draw-at {:x @x :y @y} \*))))
    ;(for [i (range w)
    ;      j (range h)]
    ;  "x")))

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

(defn redraw-diff []
  (let [es (empty-screen screen-w screen-h)
        s (reduce #(draw-at %1 %2 \X) es (p/get-players-property :coords))
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
                    :health 5
                    :term {:w 0
                           :h 0}})))



(defn input [client c]
  (println c)
  (case c
    :arrow-down (p/alter-player [client :coords :y] inc)
    :arrow-up (p/alter-player [client :coords :y] dec)
    :arrow-left (p/alter-player [client :coords :x] dec)
    :arrow-right (p/alter-player [client :coords :x] inc)
    nil))

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

;(def server
;  (ts/create-server 5073
;     {:connect  connect
;      :line     line
;      :input    input
;      :resize   resize
;      :close    (fn [client])
;      :shutdown (fn [server]
;                  (println "Bye!"))}))
;
(defn start-redraw-thread []
  (doto
   (Thread. redraw-thread)
   (.setDaemon true)
   (.start)))
