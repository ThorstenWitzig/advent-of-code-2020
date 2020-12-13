(ns aoc.day6a
  (:require [clojure.string :as string]
            [clojure.set :as set]))

;Rewritten to save chars using high-order (wait a second, my university is calling)
(defn apply-set-function [f lines]
  (->> lines
       (map set)
       (reduce f)
       count))

(defn count-input-set-function [f s]
  (->> (string/split s #"\n\n")
       (map string/split-lines)
       (map (partial apply-set-function f))
       (reduce +)))

;Part 1
(def count-input-union (partial count-input-set-function set/union)) ;partial is needed to make use of currying in clojure. Maybe I should have used Haskell...
;Part 2
(def count-input-intersection (partial count-input-set-function set/intersection))
