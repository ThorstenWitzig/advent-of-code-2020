(ns aoc.day22
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn score [lis]
  (if (empty? lis)
    0
    (+ (* (first lis) (count lis))
       (score (rest lis)))))

(defn round [p1 p2]
  (cond
    (empty? p1) (score p2)
    (empty? p2) (score p1)
    (> (first p1) (first p2)) (round (concat (rest p1) (reverse (sort [(first p1) (first p2)])))
                                     (rest p2))
    :else (round (rest p1)
                 (concat (rest p2) (reverse (sort [(first p1) (first p2)]))))))

; Part 2
(defn had-happend-before? [p1 p2 rounds-before]
  (some #(and (= p1 (first %))
              (= p2 (second %)))
        rounds-before))

(defn round-recursive [p1 p2 rounds-before]
  (cond
    (had-happend-before? p1 p2 rounds-before) {:winner "p1" :score (score p1)}
    (empty? p1) {:winner "p2" :score (score p2)}
    (empty? p2) {:winner "p1" :score (score p1)}

    (or (> (first p1) (count (rest p1)))
        (> (first p2) (count (rest p2))))
    (if (> (first p1) (first p2))
      (recur (concat (rest p1) (reverse (sort [(first p1) (first p2)])))
             (rest p2)
             (cons [p1 p2] rounds-before))
      (recur (rest p1)
             (concat (rest p2) (reverse (sort [(first p1) (first p2)])))
             (cons [p1 p2] rounds-before)))

    :else (let [win (round-recursive (take (first p1) (rest p1))
                                     (take (first p2) (rest p2))
                                     [])]
            (if (= "p1" (:winner win))
              (recur (concat (rest p1) [(first p1) (first p2)])
                     (rest p2)
                     (cons [p1 p2] rounds-before))
              (recur (rest p1)
                     (concat (rest p2) [(first p2) (first p1)])
                     (cons [p1 p2] rounds-before)))))
  )
