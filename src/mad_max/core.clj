(ns mad-max.core
  (:require [mad-max.controller :as controller]
            [mad-max.server :as server]
            [mad-max.controller2 :as controller2]))

(defn start []
  (controller/initialize)
  (server/start-server))

(defn stop []
  (server/stop-server @server/telnet-server)
  (controller/reset-all-state))

(defn restart []
  (stop)
  (start))

(defn start2 []
  (controller2/main-loop))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
