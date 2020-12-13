(ns aoc.day3
  (:require [clojure.string :as string]))

(defn count-line [line pos]
  (if (= \#            ;Sadly, clojure makes a different between a char and a one sized string, so "#" and \# are different things
         (nth line (mod pos (count line))))                 ; nth returns the nth element of a list. Strings are a list of chars.
    1
    0))

(defn count-lines [lines pos offset-x offset-y]
  (if (empty? lines)
    0
    (+ (count-line (first lines)
                   pos)
       (count-lines (drop offset-y lines)
                    (+ pos offset-x)
                    offset-x
                    offset-y))))

(defn count-all-slopes [input]
  (let [lines (drop 1 (string/split-lines input))]
    (* (count-lines lines 1 1 1)
       (count-lines lines 3 3 1)
       (count-lines lines 5 5 1)
       (count-lines lines 7 7 1)
       (count-lines (drop 1 lines) 1 1 2))))
