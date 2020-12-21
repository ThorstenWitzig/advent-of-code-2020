(ns aoc.day21
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn remove-whitespaces
  "Removes whitespaces from a string"
  [s]
  (some-> s
          (string/replace "\u00A0" " ")
          (string/replace "\uE202" " ")
          string/trim
          (string/replace "  " " ")
          (string/replace "  " " ")))

(defn extract-before [s c]
  (some-> s
          (string/split (re-pattern c))
          first
          remove-whitespaces))

(defn extract-after-first [s c]
  (some->> s
           (#(string/split % (re-pattern c)))
           rest
           (string/join ":")
           remove-whitespaces))

;Part 1
(defn read-food [line]
  {:ingredients (-> (extract-before line #"\(contains")
                    (string/split #" "))
   :allergenes  (-> (extract-after-first line "contains")
                    (extract-before #"\)")
                    (string/split #", "))})

(defn get-ingredients-with-allergene [foods allergene]
  (->> foods
       (filter #(some #{allergene} (:allergenes %)))
       (map #(set (:ingredients %)))
       (reduce set/intersection)
       ))

(defn all-ingredients [foods]
  (->> foods
       (map :ingredients)
       (map set)
       (reduce set/union)))

(defn get-foods-from-input [input]
  (->> input
       string/split-lines
       (map read-food)))

(defn find-non-allergenes-ingredients [foods]
  (let [allergenes (->> foods
                        (map :allergenes)
                        flatten
                        distinct)]
    (->> allergenes
         (map #(get-ingredients-with-allergene foods %))
         (reduce set/union)
         (set/difference (all-ingredients foods))
         )))

(defn count-non-allergenes-ingredients-appearances [input]
  (let [foods (get-foods-from-input input)
        ings (find-non-allergenes-ingredients foods)]
    (->> foods
         (map :ingredients)
         (map (fn [x] (filter #(some #{%} ings)x)))
         (flatten)
         count
         )))

;Part 2 As I'm lazy, the result is calculated using pencil and paper
(defn create-allergene-map [foods]
  (let [allergenes (->> foods
                        (map :allergenes)
                        flatten
                        distinct)]
    (->> allergenes
         (map (fn [x] {x (get-ingredients-with-allergene foods x)})))))

