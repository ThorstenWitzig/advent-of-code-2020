(ns aoc.day12
  (:require [clojure.string :as string]))

;Part 1
(defn go-step [[cmd & cmds] x y r]
  (if (nil? cmd)
    (+ (Math/abs x) (Math/abs y))
    (let [c      (first cmd)
          amount (read-string (subs cmd 1))]
      (cond
        (= c \S) (go-step cmds x (+ y amount) r)
        (= c \N) (go-step cmds x (- y amount) r)
        (= c \E) (go-step cmds (+ x amount) y r)
        (= c \W) (go-step cmds (- x amount) y r)
        (= c \L) (go-step cmds x y (- r amount))
        (= c \R) (go-step cmds x y (+ r amount))
        (= c \F) (condp = (mod r 360)
                   0   (go-step cmds (+ x amount) y r)
                   180 (go-step cmds (- x amount) y r)
                   90  (go-step cmds x (+ y amount) r)
                   270 (go-step cmds x (- y amount) r))))))

(defn fly-high [input]
  (go-step (string/split-lines input) 0 0 0))


;Part 2
(defn go-step-waypoint [[cmd & cmds] x y w-x w-y r]
  (if (nil? cmd)
    (+ (Math/abs x) (Math/abs y))
    (let [c      (first cmd)
          amount (read-string (subs cmd 1))]
      (cond
        (= c \N) (go-step-waypoint cmds x y w-x (- w-y amount) r)
        (= c \S) (go-step-waypoint cmds x y w-x (+ w-y amount) r)
        (= c \E) (go-step-waypoint cmds x y (+ w-x amount) w-y r)
        (= c \W) (go-step-waypoint cmds x y (- w-x amount) w-y r)
        (= c \R) (condp = (mod amount 360)
                   0 (go-step-waypoint cmds x y w-x w-y r)
                   180 (go-step-waypoint cmds x y (* -1 w-x) (* -1 w-y) r)
                   90 (go-step-waypoint cmds x y (* -1 w-y) w-x r)
                   270 (go-step-waypoint cmds x y w-y (* -1 w-x) r))
        (= c \F) (go-step-waypoint cmds
                                   (+ x
                                      (* amount w-x))
                                   (+ y
                                      (* amount w-y))
                                   w-x
                                   w-y
                                   r)))))

(defn go-waypoints [l]
  (go-step-waypoint l 0 0 10 -1 0))

; NOOO, you can't just replace left turns by right turns!
; Haha, L goes RRRRRRRRRRR
(defn replace-left [cmd]
  (-> cmd
      (string/replace "L90" "R270")
      (string/replace "L180" "R180")
      (string/replace "L270" "R90")))

(defn fly-away [input]
  (->> (string/split-lines input)
       (map replace-left)
       go-waypoints))

