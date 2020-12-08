(ns aoc.day6
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn count-chars-union [lines]
  (->> lines
       (map set)
       (reduce set/union)
       count))

(defn count-input-union [s]
  (->> (string/split s #"\n\n")
       (map string/split-lines)
       (map count-chars-union)
       (reduce +)))

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
