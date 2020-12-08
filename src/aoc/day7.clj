(ns aoc.day7
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn listify-string
  "If parameter is a string, returns a list of this string, otherwise returns the unmodified list"
  [x]
  (if (string? x)
    [x]
    x))
(def matcher (partial re-matcher #"\d+"))


(defn extract-number
  "Extract first number from a string"
  [s]
  (if s
    (if-let [number (some-> s
                            listify-string
                            first
                            matcher
                            re-find)]
      number
      (some-> s
              listify-string
              first
              matcher
              re-find))))

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


(defn parse-line [line]
  (let [in   (extract-before line "bags contain")
        outs (-> (extract-after-first line "bags contain")
                 (string/split #","))]
    {in outs}))

(defn parse-input [input]
  (->> (string/split-lines input)
       (map parse-line)
       (into {})))

(defn list-has-goal [l goal]
  (some #(string/includes? % goal) l))

(defn clean [s]
  (if (string/includes? s "no other bag")
    s
    (extract-after-first (extract-before s "bag") (extract-number s))))

(defn find-path [m bag goal]
  (if (string/includes? bag "no other bags")
    nil
    (or (list-has-goal (get m bag) goal)
        (some identity (map #(find-path m (clean %) goal) (get m bag))))))

(defn count-path [m bag]
  (if (string/includes? bag "no other bags")
    0
    (->> (get m bag)
         (remove #(string/includes? % "no other bag"))
         (map (fn [x] {:c (read-string (extract-number x)) :n (clean x)}))
         (map #(+ (:c %)
                  (* (:c %) (count-path m (:n %)))))
         (reduce +))))

(defn find-all [input]
  (let [m (parse-input input)]
    (->> (keys m)
         (filter #(find-path m % "shiny gold"))
         count)))


(defn count-all [input]
  (let [m (parse-input input)]
    (count-path m "shiny gold")))
