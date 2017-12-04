(ns mad-max.server
  (:require [tenlet.server :as ts]
            [tenlet.escape :as es]
            [mad-max.actions :as actions]
            [mad-max.util :as util])
  (:import (java.net ServerSocket)))

(def telnet-server (atom nil))

(defn initialize-client [client]
  (ts/write client (str es/IAC es/DO es/LINE))
  (ts/write client (str es/IAC es/DO es/NAWS))
  (ts/write client (str es/IAC es/WILL es/ECHO))
  (ts/write client es/CLR))

(defn send-full-frame [client frame]
  (ts/write client frame))

(defn connect [client]
  (initialize-client client)
  (actions/enqueue {:type   :make-and-add-player
                    :client client}))

(defn line [client string])

(defn input [client ch])

(defn resize [client {:keys [w h]}]
  (actions/enqueue {:type   :update-client
                    :client client
                    :data   {:window {:width  w
                                      :height h}}}))

(defn close [client])

(defn shutdown [^ServerSocket server]
  (util/debug-print "Shutting down")
  (actions/enqueue {:type :stop-controller})
  (.close server))

(defn start-server []
  (reset! telnet-server
          (ts/create-server 5073
                      {:connect  connect
                       :line     line
                       :input    input
                       :resize   resize
                       :close    close
                       :shutdown shutdown})))

(defn stop-server [^ServerSocket server]
  (util/debug-print "Stopping server")
  (actions/enqueue {:type :stop-controller})
  (if (nil? server)
    (util/debug-print "No server running, can't shut down")
    (.close server)))

(defn restart-server [telnet-server]
  (stop-server @telnet-server)
  (start-server))
