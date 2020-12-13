(ns aoc.day04
  (:require [clojure.string :as string]
            [clojure.set :as set]))


(def required-fields (set (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))

(defn read-passport [p]
  (->> (string/split p #"\s")
      (map #(string/split % #":"))))                        ; map applies a function to every element of the list

;Part 1
(defn check-passport [p]
  (->> (read-passport p)
       (map first)
       set
       (set/intersection required-fields)                   ;sets are luckily build in Clojure
       (= required-fields)))

(defn check-count-all-passports [input]
  (->> (string/split input #"\n\n")
       (filter check-passport)
       count))

;Part 2
(defn check-height [hgt]                                    ;Yes, I could have used regex more often for this puzzle, but hey, its hard to think in the morning!
  (cond
    (nil? hgt) nil
    (string/ends-with? hgt "cm") (and (= (count hgt) 5)
                                      (<= 150 (read-string (subs hgt 0 3)) 193))
    (string/ends-with? hgt "in") (and (= (count hgt) 4)
                                      (<= 59 (read-string (subs hgt 0 2)) 76))))

(defn check-passwort-values [p]
  (println p)
  (let [passport (->> (read-passport p)
                      (map (fn [x] {(keyword (first x)) (last x)}))
                      (into {}))]
    (and (<= 1920 (read-string (or (:byr passport) "0")) 2002)
         (<= 2010 (read-string (or (:iyr passport) "0")) 2020)
         (<= 2020 (read-string (or (:eyr passport) "0")) 2030)
         (check-height (:hgt passport))
         (and (= (count (:hcl passport)) 7)
              (re-matches #"\#([0-9]|[a-f])*" (:hcl passport))) ; But sometimes regexes might be easier...
         (some #{(:ecl passport) } (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth")) ;some is used to check if a list contains an element
         (and (= 9 (count (:pid passport)))
              (re-matches #"\d*" (:pid passport))))))

(defn check-value-all-passports [input]
  (->> (string/split input #"\n\n")
       (filter check-passwort-values)
       count))
