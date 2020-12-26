(ns aoc.day19
  (:require [clojure.string :as string]))

; Why is it, when something with substrings happens, it is always you three?
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
(defn parse-solved [line]
  {:solved (list (extract-before (extract-after-first line "\"") "\""))})

(defn parse-sublist [line]
  {:sublist (map read-string (string/split line #" "))})

(defn parse-or [line]
  (let [parts (map remove-whitespaces (string/split line #"\|"))]
    {:a (parse-sublist (first parts))
     :b (parse-sublist (second parts))}))

(defn parse-rule [line]
  (cond
    (string/includes? line "\"") (parse-solved line)
    (string/includes? line "|") (parse-or line)
    :else (parse-sublist line)))

(defn parse-rules [input]
  (let [rules-unparsed (-> (string/split input #"\n\n")
                           first
                           string/split-lines)]
    (->> rules-unparsed
         (map (fn [x] {(read-string (extract-before x ":")) (parse-rule (extract-after-first x ":"))}))
         (into {}))))

(defn kreuzprodukt [l1 l2]
  (flatten (map (fn [x]
                  (map (fn [y]
                         (str x y))
                       l2))
                l1)))

(defn kreuzprodukt-list [lis]
  (reduce kreuzprodukt lis))

(defn replace-with-solved [rule-map value]
  (if-let [s (:solved (get rule-map value))]
    {:solved s}
    value))

(defn solve-rule [rule rule-map]
  (cond
    (:solved rule) rule
    (:sublist rule) (cond
                      (every? :solved (:sublist rule))
                      {:solved (kreuzprodukt-list (map #(:solved %) (:sublist rule)))}

                      :else {:sublist (map #(replace-with-solved rule-map %) (:sublist rule))})
    (:a rule) (cond
                (and (:solved (:a rule))
                     (:solved (:b rule)))
                {:solved (concat (:solved (:a rule)) (:solved (:b rule)))}

                :else
                {:a (solve-rule (:a rule) rule-map)
                 :b (solve-rule (:b rule) rule-map)})))

(defn solve-for-rule [pos rule-map]
  (let [rule (get rule-map pos)]
    (assoc
      rule-map
      pos
      (solve-rule rule rule-map))))

(defn step-solve-rules [rule-map]
  (reduce #(solve-for-rule %2 %1) rule-map (keys rule-map)))

(defn solve-rules [rule-map]
  (let [new-step (step-solve-rules rule-map)
        solved-count (->> new-step
                          (vals)
                          (map :solved)
                          (remove nil?)
                          count)]
    (if (= (count (vals rule-map))
           solved-count)
      new-step
      (recur new-step))))

(defn get-allowed-strings [solved-rules]
  (:solved (get solved-rules 0)))

(defn check-input [input]
  (let [rules-raw (parse-rules input)
        solved-rules (solve-rules rules-raw)
        allowed-strings (get-allowed-strings solved-rules)
        messages (-> (string/split input #"\n\n")
                     second
                     string/split-lines)]
    (->> messages
         (filter #(some #{%} allowed-strings))
         count)))

; Part 2
(defn match-part-r42-r31 [part r42 r31]
  (cond
    (some #{part} r42) "r42"
    (some #{part} r31) "r31"
    :else "invalid"))

(defn match-r42-r31 [word r42 r31]
  (println word)
  (let [checked-word (->> (partition 8 word)
                          (map string/join)
                          (map #(match-part-r42-r31 % r42 r31)))
        r42-count    (count (filter #(= "r42" %) checked-word))]
    (println checked-word)
    (let [result (and (not (some #{"invalid"} checked-word))
                      (> (* 2 r42-count) (count checked-word))
                      (every? #(= "r42" %) (take r42-count checked-word))
                      (< r42-count (count checked-word)))]
      (println result)
      result)))

(defn check-input-with-recursion [input]
  (let [solved-rules (->> input
                          parse-rules
                          solve-rules)
        r42 (:solved (get solved-rules 42))
        r31 (:solved (get solved-rules 31))]
    (->> (string/split input #"\n\n")
         second
         string/split-lines
         (filter #(match-r42-r31 % r42 r31))
         count)))
