(ns aoc.day6
  (:require [clojure.string :as string]
            [clojure.set :as set]))

;Part 1
(defn count-chars-union [lines]
  (->> lines
       (map set)
       (reduce set/union) ; Reduce might also be known as fold. It takes a function and a list (and an optional starting value) and applies the function to one element the last result for the whole list
       count))                                              ; e.g. (reduce + (list 1 2 3 4)) equals (+ (+ (+ 1 2) 3) 4)

(defn count-input-union [s]
  (->> (string/split s #"\n\n")
       (map string/split-lines)
       (map count-chars-union)
       (reduce +)))

; Part 2. Copy & Paste.
(defn count-chars-intersection [lines]
  (->> lines
       (map set)
       (reduce set/intersection)
       count))

(defn count-input-intersection [s]
  (->> (string/split s #"\n\n")
       (map string/split-lines)
       (map count-chars-intersection)
       (reduce +)))
