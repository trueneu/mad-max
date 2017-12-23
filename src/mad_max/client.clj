(ns mad-max.client)

(defn make-client [player-id]
  {:state :entering-name
   :window {:width 0 :height 0}
   :player-id player-id})