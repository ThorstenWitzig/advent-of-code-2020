(ns aoc.day14
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
(defn to-binary-step [num pos]
  (if (< pos 0)
    (list)
    (let [exp (Math/pow 2 pos)]
      (if (>= num exp)
        (cons \1
              (to-binary-step (- num exp) (dec pos)))
        (cons \0
              (to-binary-step num (dec pos)))))))

(defn to-binary [num]
  (string/join (to-binary-step num 35)))

(defn from-binary-step [[bin & bins] pos]
  (if (nil? bin)
    0
    (if (= \0 bin)
      (from-binary-step bins (inc pos))
      (+ (Math/pow 2 pos)
         (from-binary-step bins (inc pos))))))

(defn from-binary [bnum]
  (from-binary-step (reverse bnum) 0))

(defn combine-bitmask [num bitmask]
  (->> (map #(if (= %1 \X) %2 %1)
            bitmask
            (to-binary num))
       string/join
       from-binary))

(defn extract-memory-address [line]
  (-> line
      (extract-before #"\]")
      (extract-after-first #"\[")
      read-string))

(defn read-step [memory bitmask [line & lines]]
  (if (nil? line)
    memory
    (if (string/starts-with? line "mask")
      (recur memory (extract-after-first line "=") lines)
      (let [masked-value (-> (extract-after-first line "=")
                             read-string
                             (combine-bitmask bitmask))
            updated-memory (merge memory
                                  {(extract-memory-address line) masked-value})]
        (recur updated-memory bitmask lines)))))

(defn read-input [input]
  (->> (string/split-lines input)
       (read-step {} "INVALID_BITMASK")
       vals
       (reduce +)))

;Part 2
(defn combine-bitmask-floating [num bitmask]
  (->> (map #(if (= %1 \0)
               %2
               %1)
            bitmask
            (to-binary num))
       (string/join)))

(defn get-all-address-step [[f & xs]]
  (if (empty? xs)
    (if (= f \X)
      [[\1] [\0]]
      [[f]])
    (let [vals (cond
                 (= f \1) [\1]
                 (= f \0) [\0]
                 :else [\0 \1])
          as (get-all-address-step xs)]
      (->> (map (fn [x]
                  (map #(conj % x)
                       as))
                vals)
           (reduce concat)))))

(defn get-all-addresses [lis]
  (->> (get-all-address-step lis)
       (map reverse)
       (map string/join)))

(defn create-memory-with [pos value bitmask]
  (->> (combine-bitmask-floating pos bitmask)
       get-all-addresses
       (map from-binary)
       (map (fn [x] {x value}))
       (into {})))

(defn read-step-floating [memory [line & lines] bitmask]
  (if (nil? line)
    memory
    (if (string/starts-with? line "mask")
      (recur memory lines (extract-after-first line "="))
      (let [updated-memory (merge memory
                                  (create-memory-with (extract-memory-address line)
                                                      (read-string (extract-after-first line "="))
                                                      bitmask))]
        (recur updated-memory lines bitmask)))))

(defn read-input-floating [input]
  (->> (read-step-floating {} (string/split-lines input) "")
       vals
       (reduce +)))
