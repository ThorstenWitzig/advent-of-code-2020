(ns aoc.day13
  (:require [clojure.string :as string]))

; Part 1
(defn check-time [arrival [bus & busses] best-time best-line]
  (cond
    (nil? bus) (* best-time best-line)
    (zero? (mod arrival bus)) 0
    (< (- bus (rem arrival bus)) best-time) (recur arrival busses (- bus (rem arrival bus)) bus)
    :else (recur arrival busses best-time best-line)))

(defn find-earliest-bus [input]
  (let [[arrival busses] (string/split-lines input)]
    (check-time
      (read-string arrival)
      (->> (string/split busses #",")
           (remove #(= % "x"))
           (map read-string))
      (Integer/MAX_VALUE)
      0)))

; Part 2a (only works on small input)
(defn find-denominator [[x xs] pos t]
  (if (nil? x)
    t
    (if (= "x" xs)
      (recur xs (inc pos) t)
      (if (= 0 (mod (+ t pos) (read-string x)))
        (recur xs (inc pos) t)
        nil))))


(defn find-timestamp-step [t lis step]
  (if-let [m (find-denominator lis 1 (* t step))]
    m
    (recur t lis (inc step))))

(defn find-common-timestamp-brute-force [input]
  (-> input
      string/split-lines
      second
      (string/split #",")
      (#(find-timestamp-step (read-string (first %)) (rest %) 1))))

; Part 2b
; Solving by using https://www.dcode.fr/chinese-remainder (Lazy way, if you don't want to implement CRT yourself
(defn calculate-rem-mod-pairs [input]
  (->> (-> input
           string/split-lines
           second
           (string/split #","))
       (map-indexed #(str "Remainder: -" %1 ", Modulo: " %2 ))
       (remove #(string/ends-with? % "Modulo: x"))
       (string/join "\n ")))
