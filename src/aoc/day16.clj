(ns aoc.day16
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
(defn parse-rules [input]
  (map (fn [x]
         {:r1   (map read-string (string/split (extract-before (extract-after-first x ":") "or") #"-"))
          :r2   (map read-string (string/split (extract-after-first (extract-after-first x ":") "or") #"-"))
          :name (extract-before x ":")})
       (string/split-lines input)))

(defn parse-input [input]
  (let [parts (string/split input #"\n\n")
        rules (parse-rules (first parts))
        my-ticket (map read-string
                       (-> (second parts)
                           (string/split-lines)
                           last
                           (string/split #",")))
        other-tickets (map #(map read-string
                                 (string/split % #","))
                           (drop 1 (string/split-lines (last parts))))]
    {:rules  rules
     :my     my-ticket
     :nearby other-tickets}))

(defn has-rule [val rules]
  (->> rules
       (map #(or (<= (first (:r1 %))
                     val
                     (last (:r1 %)))
                 (<= (first (:r2 %))
                     val
                     (last (:r2 %)))))
       (reduce #(or %1 %2))))

(defn remove-valid [lis rules]
  (remove #(has-rule % rules) lis))

(defn check-nearby-tickets [input]
  (let [parsed (parse-input input)]
    (->> (map #(remove-valid % (:rules parsed))
              (:nearby parsed))
         flatten
         (reduce +))))

;Part 2
(defn remove-invalid [lis rules]
  (filter #(has-rule % rules) lis))

(defn remove-invalid-tickets [tickets rules]
  (->> (map #(remove-invalid % rules)
            tickets)
       (remove #(not= (count %) (count rules)))))

(defn check-rule [val rule]
  (or (<= (first (:r1 rule))
          val
          (last (:r1 rule)))
      (<= (first (:r2 rule))
          val
          (last (:r2 rule)))))

(defn find-valid-rules [vals rules]
  (filter #(= (count (filter (fn [x] (check-rule x %))
                             vals))
              (count vals))
          rules))

(defn to-vals [tickets step]
  (if (>= step (count (first tickets)))
    (list)
    (cons (map #(nth % step)
               tickets)
          (to-vals tickets (inc step)))))

(defn rules-step-map [valss rules]
  (map-indexed (fn [i x] {:pos   i
                          :rules (find-valid-rules x rules)})
               valss))

(defn only-one-rule-everywhere [rule-maps]
  (->> (map :rules rule-maps)
       (map count)
       (filter #(> % 1))
       empty?))

(defn find-single-rule-names [rule-maps]
  (->> rule-maps
       (filter #(= 1 (count (:rules %))))
       (map :rules)
       (map first)
       (map :name)))

(defn remove-rules [rule-map names]
  (map (fn [rule] (if (= 1 (count (:rules rule)))
                    rule
                    (assoc rule :rules (remove #(some #{(:name %)} names) (:rules rule)))))
       rule-map))

(defn reduce-to-one [rule-maps]
  (if (only-one-rule-everywhere rule-maps)
    rule-maps
    (reduce-to-one (remove-rules rule-maps (find-single-rule-names rule-maps)))))

(defn find-valid-rules-for-pos [input]
  (let [parsed (parse-input input)
        valid-nearby (remove-invalid-tickets (:nearby parsed) (:rules parsed))
        vals (to-vals valid-nearby 0)
        rules-for-step (rules-step-map vals (:rules parsed))]
    rules-for-step))

;To be honest, I wrote this part afterwards. I used pencil and paper to solve the last part of part 2
(defn multiply-departure-values [input]
  (let [parsed (parse-input input)
        departure-pos (->> (find-valid-rules-for-pos input)
                          reduce-to-one
                          (filter #(string/includes? (:name (first (:rules %))) "departure"))
                          (map :pos))]
    (->> departure-pos
         (map #(nth (:my parsed) %))
         (reduce *))))
