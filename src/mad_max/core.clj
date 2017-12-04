(ns mad-max.core
  (:require [mad-max.controller :as controller]))

(defn initialize []
  (controller/initialize))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
