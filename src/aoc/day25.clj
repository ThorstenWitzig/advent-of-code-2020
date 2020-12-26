(ns aoc.day25
  (:require [clojure.string :as string]))

;Part 1
(defn do-encryption-loop [loop-size val subject-number]
  (if (zero? loop-size)
    val
    (recur (dec loop-size)
           (rem (* val subject-number) 20201227)
           subject-number)))


(defn find-loop-size [loop-size val subject-number goal]
  (if (= val goal)
    loop-size
    (recur (inc loop-size)
           (rem (* val subject-number) 20201227)
           subject-number
           goal)))
