(ns mad-max.core
  (:require
    ;[mad-max.controller :as controller]
    [mad-max.server :as server]
    [mad-max.controller2 :as controller2]
    [mad-max.actions :as actions]))

(defn stop-telnet-server []
  (server/stop-server @server/telnet-server))

(defn start2 []
  (doto
    (Thread. controller2/main-loop)
    (.setDaemon true)
    (.start)))

(defn stop2 []
  (actions/enqueue {:type :stop-controller})
  (Thread/sleep 100)
  (actions/q-flush))

(defn restart2 []
  (stop2)
  (start2))

(defn hard-restart2 []
  (stop-telnet-server)
  (restart2))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
