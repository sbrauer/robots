(ns robots.coord
  (:require [robots.constants :as const]))

; Coords are 2-item vectors [x y] (where top-left is [0 0])

(defn dir->offset
  [dir]
  (dir {:n [0 -1] :s [0 1] :e [1 0] :w [-1 0] :ne [1 -1] :se [1 1] :nw [-1 -1] :sw [-1 1]}))

(defn move-coord
  [coord dir]
  (map + coord (dir->offset dir)))

;; FIXME: move to board?
(defn coord-in-bounds?
  "Return true if the given coord is within the bounds specified by cols and rows."
  [[x y]]
  (and (<= 0 x)
       (<= 0 y)
       (< x const/cols)
       (< y const/rows)))

(defn move-towards
  "Given two coordinates, return a new coord that gets source one step closer to target.
  (There's no change if source is already equal to target.)"
  [target source]
  (mapv + source (map compare target source)))

;; FIXME: move to board?
(defn all-board-coords []
  (for [x (range const/cols)
        y (range const/rows)]
    [x y]))

;; FIXME: move to board?
(defn rand-coord
  []
  [(rand-int const/cols) (rand-int const/rows)])
