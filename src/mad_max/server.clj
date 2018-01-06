(ns mad-max.server
  (:require [tenlet.server :as ts]
            [tenlet.escape :as es]
            [mad-max.actions :as actions]
            [mad-max.util :as util])
  (:import (java.net ServerSocket)))

(def telnet-server (atom nil))
(def clients (ref #{}))

(defn initialize-client [client-connection]
  (ts/write client-connection (str es/IAC es/DO es/LINE))
  (ts/write client-connection (str es/IAC es/DO es/NAWS))
  (ts/write client-connection es/CLR))

(defn send-no-echo [client-connection]
  (ts/write client-connection (str es/IAC es/WILL es/ECHO)))


(defn reset-cursor [client-connection x y]
  (ts/write client-connection (es/cursor x y)))

(defn send-full-frame [client-connection frame]
  (reset-cursor client-connection 0 0)
  (ts/write client-connection frame))

(defn send-disconnect [client-connection]
  (.close client-connection))

(defn connect [client-connection]
  (dosync
    (alter clients conj client-connection))
  (initialize-client client-connection)
  (actions/enqueue {:type              :client-connect
                    :client-connection client-connection}))

(defn line [client-connection string]
  ;(util/debug-print "Received line!" string)
  (actions/enqueue {:client-connection client-connection
                    :type              :input-line
                    :input             string}))

(defn input [client-connection ch]
  ;(util/debug-print "Received input from client: " ch)
  (actions/enqueue {:client-connection client-connection
                    :type              :input-char
                    :input             ch}))

(defn resize [client-connection {:keys [w h]}]
  (actions/enqueue {:type              :update-client
                    :client-connection client-connection
                    :data              {:window {:width  w
                                                 :height h}}}))

(defn close [client-connection]
  (dosync
    (alter clients disj client-connection))
  (actions/enqueue {:type              :client-disconnect
                    :client-connection client-connection}))

(defn shutdown [^ServerSocket server]
  "Stops telnet server. Should not be invoked manually."
  (util/debug-print "Shutting down")
  (doseq [client @clients] (send-disconnect client))
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
  (if (nil? server)
    (util/debug-print "No server running, can't shut down")
    (shutdown server)))

(defn restart-server [telnet-server]
  (stop-server @telnet-server)
  (start-server))
