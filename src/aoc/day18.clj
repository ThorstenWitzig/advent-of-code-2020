(ns aoc.day18
  (:require [clojure.string :as string]))

;Part 1
(defn do-math [o x1 x2]
  (cond
    (= \* o) (* x1 x2)
    (= \+ o) (+ x1 x2)))

(defn to-num [x]
  (read-string (str x)))

(defn eval-term [last-num op [s & ss :as stack] [x & xs]]
  (cond
    (nil? x) last-num
    (or (= x \*) (= x \+)) (recur last-num x stack xs)
    (= x \() (recur nil nil (cons {:n last-num :o op} stack) xs)
    (= x \)) (recur (:n s) (:o s) ss (cons last-num xs))
    (nil? op) (recur (to-num x) op stack xs)
    :else (let [result (do-math op last-num (to-num x))]
            (recur result nil stack xs))))

(defn calculate [s]
  (eval-term nil nil [] (string/replace s " " "")))

(defn solve-all [input]
  (->> (string/split-lines input)
       (map calculate)
       (reduce +)))

; Part 2
; It's rare, but I actually like the todays approach I took very much
(defn get-subterm [[t & ts] open-braces acc]
  (cond
    (nil? t) (reduce str (reverse acc))

    (and (= \) t) (= open-braces 0))
    (reduce str (reverse (cons t acc)))

    (and (= \) t) (not= open-braces 0))
    (recur ts (dec open-braces) (cons t acc))

    (= \( t)
    (recur ts (inc open-braces) (cons t acc))

    :else (recur ts
                 open-braces
                 (cons t acc))))

(defn put-braces [s]
  (cond
    (= (count s) 0) ""

    (= \* (first s))
    (let [subterm        (get-subterm (subs s 1) 0 [])
          remaining-term (subs s (inc (count subterm)))]
      (str "*(" (put-braces subterm) ")" (put-braces remaining-term)))

    :else (str (first s) (put-braces (subs s 1)))))

(defn calculate-line-before-point [s]
  (eval-term nil nil [] (string/replace (put-braces s) " " "")))

(defn solve-all-line-before-point [input]
  (->> (string/split-lines input)
       (map calculate-line-before-point)
       (reduce +)))
