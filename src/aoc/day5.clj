(ns aoc.day5
  (:require [clojure.string :as string]))

(defn change-bounds [lower upper s]
  (let [diff (/ (- (inc upper) lower) 2)]
    (if (or (= (first s) \F)
            (= (first s) \L))
      (if (empty? (rest s))
        lower
        (change-bounds lower (- upper diff) (rest s)))
      (if (empty? (rest s))
        upper
        (change-bounds (+ lower diff) upper (rest s))))))

(defn calc-seat-id [i]
  (+ (* 8 (change-bounds 0 127 (subs i 0 7)))
     (change-bounds 0 7 (subs i 7))))

(defn find-seat [last-id ids]
  (if (and (=  last-id
               (- (first ids) 2))
           (= (second ids)
              (inc (first ids)))
           )
    (dec (first ids))
    (find-seat (first ids)
               (rest ids))))
