(ns aoc.day23
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn find-n-or-highest [n lis-sorted]
  (cond
    (< n (first lis-sorted)) (last lis-sorted)
    (some #{n} lis-sorted) n
    :else (find-n-or-highest (dec n) lis-sorted)))

(defn find-n-or-highest-optimized [n excepts max-number]
  (cond
    (= n 0) max-number
    (some #{n} excepts) (recur (dec n) excepts max-number)
    :else n))

(defn move-cups [maximum [current one two three & remaining]]
  (let [next-cup (find-n-or-highest-optimized (dec current) [one two three] maximum)
        between (take-while #(not= next-cup %) remaining)
        after (rest (drop-while #(not= next-cup %) remaining))]
    (concat between
            [next-cup one two three]
            after
            [current])))

(defn move-cups-optimized [maximum v]
  (let [current (first v)
        threes (subvec v 0 3)
        remaining (subvec v 3)
        next-cup (find-n-or-highest-optimized (dec current) threes maximum)
        pos (.indexOf remaining next-cup)
        between (subvec remaining 0 (inc pos))
        after (subvec remaining pos)]
    (vec (concat between
                 threes
                 after
                 [current]))))

(defn do-n-moves [moves maximum cups]
  (println moves)
  (if (= moves 0)
    cups
    (recur (dec moves) maximum (move-cups-optimized maximum cups))))

(defn add-cups-to-million [cups]
  (vec (concat cups
               (range (inc (count cups)) 1000001))))

(defn do-a-crappy-game [cups]
  (->> cups
       (add-cups-to-million)
       (do-n-moves 10000000 1000000)
       (drop-while #(not= 1 %))
       (take 3)))
