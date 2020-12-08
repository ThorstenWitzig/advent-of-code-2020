(ns aoc.day2
  (:require [clojure.string :as string]))

(defn split-password [line]
  (let [pos-colon (string/index-of line ":")
        pos-dash  (string/index-of line "-")]
    {:password (->> pos-colon
                    inc
                    (subs line)
                    string/trim)
     :char     (first (subs line
                            (dec pos-colon)
                            pos-colon))
     :min      (read-string (subs line 0 pos-dash))
     :max      (read-string (subs line (inc pos-dash) (- pos-colon 2)))}))

(defn check-password-count [line]
  (let [pw (split-password line)]
    (<= (:min pw)
       (count (filter #(= % (:char pw)) (:password pw)))
       (:max pw))))

(defn check-passwords-count [input]
  (->> input
       string/split-lines
       (filter check-password-count)
       count))

(defn xor [f s]
  (or (and f (not s))
      (and (not f) s)))

(defn check-password-position [line]
  (let [pw (split-password line)]
    (xor (= (nth (:password pw)
                 (dec (:min pw)))
            (:char pw))
         (= (nth (:password pw)
                 (dec (:max pw)))
            (:char pw)))))

(defn check-passwords-position [input]
  (->> input
       string/split-lines
       (filter check-password-position)
       count))
