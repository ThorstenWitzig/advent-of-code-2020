(ns aoc.day5
  (:require [clojure.string :as string]))

; Yes, this puzzle could have been way easier, if interpreted as binary numbers.
; But I left it this way to document how little I think sometimes when its 6 a.m.

;Part 1
(defn change-bounds [lower upper [seat & seats]]            ; Here destructuring is used as well. The third argument is a list
  (let [diff (/ (- (inc upper) lower) 2)]                   ; where seat is the first element in the list and seats the remaining elements
    (if (or (= seat \F)                                     ; if the list is empty, seat is nil. If there are no nils in the list, its way to find out if the list is empty
            (= seat \L))
      (if (nil? seat)
        lower
        (change-bounds lower (- upper diff) seats))
      (if (empty? seats)
        upper
        (change-bounds (+ lower diff) upper seats)))))

(defn calc-seat-id [i]
  (+ (* 8 (change-bounds 0 127 (subs i 0 7)))
     (change-bounds 0 7 (subs i 7))))

(defn find-highest-seat [input]
  (->> input
       string/split-lines
       (map calc-seat-id)
       (reduce max)))

;Part 2
(defn find-seat-step [last-id ids]
  (if (and (= last-id
              (- (first ids) 2))
           (= (second ids)
              (inc (first ids))))
    (dec (first ids))
    (find-seat-step (first ids)
                    (rest ids))))

(defn find-seat-input [input]
  (let [[id & ids] (->> input
                 string/split-lines
                 (map calc-seat-id)
                 sort)]
    (find-seat-step id ids)))
