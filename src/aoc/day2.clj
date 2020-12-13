(ns aoc.day2
  (:require [clojure.string :as string]))

;Part 1
(defn split-password [line]
  (let [pos-colon (string/index-of line ":")                ;let is used to create local variables (note: Variables are read-only in clojure)
        pos-dash (string/index-of line "-")]
    ; {} creates a (empty) map in Clojure. A map is justs pairs of keys and values
    ; :something is often used as keys as this is also a function consuming a map and returning this value.
    ; e.g. if you have a map m = {:a "value"}, (:a m) would return "value" (same as (get m :a))
    ; Yes, functional programming is about using as little chars as possible!
    {:password (->> pos-colon
                    inc                                     ;inc increases a number by one, dec decreases by one
                    (subs line)                             ;subs returns a sublist of a string, starting at pos n (and has an optional end position)
                    string/trim)
     :char     (first (subs line
                            (dec pos-colon)
                            pos-colon))
     :min      (read-string (subs line 0 pos-dash))         ; read-string reads value, e.g. "5" becomes 5. Has weired side-effects, use it in production with caution
     :max      (read-string (subs line (inc pos-dash) (- pos-colon 2)))}))

(defn check-password-count [line]
  (let [pw (split-password line)]
    (<= (:min pw)                                           ;Cool thing: >, <, >=, <= can have more than two parameters, writing boundaries so much nicer!
        (count (filter #(= % (:char pw)) (:password pw)))
        (:max pw))))

(defn check-passwords-count [input]
  (->> input
       string/split-lines
       (filter check-password-count)
       count))

;Part 2
(defn check-password-position [line]
  (let [{pw :password min :min max :max char :char} (split-password line)] ; here destructuring is used to put map values directly into new variables instead of calling e.g. (:password m) all the time
    (not= (= (nth pw
                  (dec min))
             char)
          (= (nth pw
                  (dec max))
             char))))

(defn check-passwords-position [input]
  (->> input
       string/split-lines
       (filter check-password-position)
       count))
