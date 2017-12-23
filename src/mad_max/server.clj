(ns mad-max.server
  (:require [tenlet.server :as ts]
            [tenlet.escape :as es]
            [mad-max.actions :as actions]
            [mad-max.util :as util])
  (:import (java.net ServerSocket)))

(def telnet-server (atom nil))
(def clients (ref #{}))


(defn initialize-client [client]
  (ts/write client (str es/IAC es/DO es/LINE))
  (ts/write client (str es/IAC es/DO es/NAWS))
  (ts/write client es/CLR))

(defn send-no-echo [client]
  (ts/write client (str es/IAC es/WILL es/ECHO)))
  ;(ts/write client es/CLR))

(defn reset-cursor [client x y]
  (ts/write client (es/cursor x y)))

(defn send-full-frame [client frame]
  (reset-cursor client 0 0)
  (ts/write client frame))

(defn send-disconnect [client]
  (.close client))

(defn connect [client]
  (dosync
    (alter clients conj client))
  (initialize-client client)
  (actions/enqueue {:type   :client-connect
                    :client client}))

(defn line [client string]
  (util/debug-print "Received line!" string)
  (actions/enqueue {:client client
                    :type :input-line
                    :input string}))

(defn input [client ch]
  (util/debug-print "Received input from client: " ch)
  (actions/enqueue {:client client
                    :type :input-char
                    :input ch}))

(defn resize [client {:keys [w h]}]
  (actions/enqueue {:type   :update-client
                    :client client
                    :data   {:window {:width  w
                                      :height h}}}))

(defn close [client]
  (dosync
    (alter clients disj client))
  (actions/enqueue {:type   :client-disconnect
                    :client client}))

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
