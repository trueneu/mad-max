(ns mad-max.protos)

(defprotocol IMoveable
  (get-coords [IMoveable])
  (move [IMoveable direction]))

(defprotocol IVisible
  (get-representation [IVisible]))

(defprotocol ICoords
  (change [ICoords direction]))

(defprotocol IArena
  (cell-empty? [IArena ICoords]))

