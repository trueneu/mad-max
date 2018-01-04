(ns mad-max.game)

(defn make-game []
  {:arenas {}
   :entities {}
   :clients {}
   :special-entities-type-to-id {}
   :arena-id 0
   :entity-id 0})