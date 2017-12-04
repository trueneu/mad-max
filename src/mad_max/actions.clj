(ns mad-max.actions
  (:import (clojure.lang PersistentQueue)))

(def queue (ref PersistentQueue/EMPTY))

(defmethod print-method PersistentQueue [q, w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(defn enqueue [item]
  (dosync
    (alter queue conj item)))

(defn dequeue []
  (dosync
    (let [item (peek @queue)]
      (alter queue pop)
      item)))
