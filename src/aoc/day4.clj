(ns aoc.day4
  (:require [clojure.string :as string]
            [clojure.set :as set]))


(def required-fields (set (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))

(defn read-passport [p]
  (->> (string/split p #"\s")
      (map #(string/split % #":"))))

(defn check-passport [p]
  (->> (read-passport p)
       (map first)
       set
       (set/intersection required-fields)
       (= required-fields)))

(defn check-height [hgt]
  (if (string/ends-with? hgt "cm")
    (and (= (count hgt) 5)
         (<= 150 (read-string (subs hgt 0 3)) 193))
    (if (string/ends-with? hgt "in")
      (and (= (count hgt) 4)
           (<= 59 (read-string (subs hgt 0 2)) 76))
      false)))

(defn check-passwort-values [p]
  (let [passport (->> (read-passport p)
                      (map (fn [x] {(keyword (first x)) (last x)}))
                      (into {}))]
    (and (<= 1920 (read-string (:byr passport)) 2002)
         (<= 2010 (read-string (:iyr passport)) 2020)
         (<= 2020 (read-string (:eyr passport)) 2030)
         (check-height (:hgt passport))
         (and (= (count (:hcl passport)) 7)
              (re-matches #"\#([0-9]|[a-f])*" (:hcl passport)))
         (some #{(:ecl passport) } (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
         (and (= 9 (count (:pid passport)))
              (re-matches #"\d*" (:pid passport))))))
