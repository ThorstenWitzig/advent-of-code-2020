(ns aoc.day8
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

(defn extract-after-first [s c]
  (some->> s
           (#(string/split % (re-pattern c)))
           rest
           (string/join ":")
           remove-whitespaces))

(defn run-program [prg line-number acc cmds]
  (if (some #{line-number} cmds)
    acc
    (let [line (nth prg line-number)]
      (cond
        (string/starts-with? line "acc") (run-program prg
                                                      (inc line-number)
                                                      (+ acc (read-string (extract-after-first line " ")))
                                                      (conj cmds line-number))
        (string/starts-with? line "nop") (run-program prg
                                                      (inc line-number)
                                                      acc
                                                      (conj cmds line-number))
        (string/starts-with? line "jmp") (run-program prg
                                                      (+ line-number (read-string (extract-after-first line " ")))
                                                      acc
                                                      (conj cmds line-number))))))

(defn start-prg [input]
  (run-program (string/split-lines input)
               0
               0
               (list)))

(defn run-program-terminating [prg line-number acc cmds]
  (if (some #{line-number} cmds)
    nil
    (if (>= line-number (count prg))
      acc
      (let [line (nth prg line-number)]
        (cond
          (string/starts-with? line "acc") (run-program-terminating prg
                                                        (inc line-number)
                                                        (+ acc (read-string (extract-after-first line " ")))
                                                        (conj cmds line-number))
          (string/starts-with? line "nop") (run-program-terminating prg
                                                        (inc line-number)
                                                        acc
                                                        (conj cmds line-number))
          (string/starts-with? line "jmp") (run-program-terminating prg
                                                        (+ line-number (read-string (extract-after-first line " ")))
                                                        acc
                                                        (conj cmds line-number)))))))
(defn change-line [line]
  (cond
    (string/starts-with? line "acc") line
    (string/starts-with? line "nop") (string/replace line "nop" "jmp")
    (string/starts-with? line "jmp") (string/replace line "jmp" "nop")))

(defn change-prg-at [prg line-number]
  (concat (take line-number prg)
          (list (change-line (nth prg line-number)))
          (drop (inc line-number) prg)))

(defn try-prgs [prg changed-line]
  (if-let [result (run-program-terminating (change-prg-at prg changed-line)
                                           0
                                           0
                                           (list))]
    result
    (try-prgs prg (inc changed-line))))

(defn run-try-prgs [input]
  (try-prgs (string/split-lines input) 0))
