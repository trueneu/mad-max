(ns mad-max.coords)

(def dir-to-movement {:up [dec :y]
                      :down [inc :y]
                      :left [dec :x]
                      :right [inc :x]})

(defn change [coords direction]
  (update coords (second (dir-to-movement direction))
                 (first (dir-to-movement direction))))
