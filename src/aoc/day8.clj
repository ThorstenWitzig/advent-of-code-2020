(ns aoc.day8
  (:require [clojure.string :as string]))

; It was useful last day, so why not use it again?
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

;Part 1
(defn run-program [prg line-number acc cmds]
  (if (some #{line-number} cmds)
    acc
    (let [line (nth prg line-number)]
      (cond ; Syntax-Sugar for nested ifs, Pair of conditions and code to execute if condition matches
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

;Part 2
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
                                                        (conj cmds line-number)) ; conj adds an element to a list (creating a new one, of course, its functional programming after all!)
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
  (concat (take line-number prg)                            ; take returns a new list of the first n elements of a list. Concat puts lists together
          (list (change-line (nth prg line-number)))        ; (list) creates a new list, e.g (list 1 2 3) create a list containing these 3 elements
          (drop (inc line-number) prg)))                    ; drop returns a new list by dropping the first n elements of a list. Yes, (concat (take n lis) (drop n lis)) = lis

(defn try-prgs [prg changed-line]
  (if-let [result (run-program-terminating (change-prg-at prg changed-line)
                                           0
                                           0
                                           (list))]
    result
    (try-prgs prg (inc changed-line))))

(defn run-try-prgs [input]
  (try-prgs (string/split-lines input) 0))
