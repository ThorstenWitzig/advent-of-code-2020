(ns aoc.day11
  (:require [clojure.string :as string]))


; *sing* This is the circle of (the game of) life
; Part 1
; No, this days solution isn't nice, but it works, bitches!
; Its slow, too.
(defn is-occupied [a pos-x pos-y]
  (if (or (< pos-x 0)
          (>= pos-x (count (first a)))
          (< pos-y 0)
          (>= pos-y (count a)))
    0
    (if (= \# (nth (nth a pos-y) pos-x))
      1
      0)))

(defn count-occupied [a pos-x pos-y]
  (+ (is-occupied a (dec pos-x) pos-y)
     (is-occupied a (inc pos-x) pos-y)
     (is-occupied a pos-x (inc pos-y))
     (is-occupied a (inc pos-x) (inc pos-y))
     (is-occupied a (dec pos-x) (inc pos-y))
     (is-occupied a pos-x (dec pos-y))
     (is-occupied a (inc pos-x) (dec pos-y))
     (is-occupied a (dec pos-x) (dec pos-y))))

(defn next-step-hull [a pos-x pos-y acc-row acc-all]
  (cond
    (>= pos-x (count (first a)))
    (recur a 0 (inc pos-y) (list) (cons (reverse acc-row) acc-all)) ; cons is equivalent to conj, but it guarantees to keep the order. Reverse is needed nevertheless

    (>= pos-y (count a)) (reverse acc-all)

    (and (= \L (nth (nth a pos-y) pos-x))
         (zero? (count-occupied a pos-x pos-y)))
    (recur a (inc pos-x) pos-y (cons \# acc-row) acc-all)

    (and (= \# (nth (nth a pos-y) pos-x))
         (>= (count-occupied a pos-x pos-y) 4))
    (recur a (inc pos-x) pos-y (cons \L acc-row) acc-all)

    :else
    (recur a (inc pos-x) pos-y (cons (nth (nth a pos-y) pos-x) acc-row) acc-all)))

(defn next-step [a]
  (next-step-hull a 0 0 (list) (list)))

(defn count-occupied-seats [step]
  (->> step
       (map #(->> %
                  (filter (fn [x] (= x \#)))
                  count))
       (reduce +)))

(defn count-all-steps [step]
  (let [new-step (next-step step)]
    (if (= step new-step)     ; Unlike to inferior languages, you can use = to check for equality for all the things in Clojure!
      (count-occupied-seats step)
      (count-all-steps new-step))))

(defn start-count-all-steps [input]
  (count-all-steps (string/split-lines input)))




; Part 2, Copy and Paste FTW
(defn is-occupied-2 [a pos-x pos-y x y]
  (if (or (< pos-x 0)
          (>= pos-x (count (first a)))
          (< pos-y 0)
          (>= pos-y (count a)))
    0
    (if (= \# (nth (nth a pos-y) pos-x))
      1
      (if (= \L (nth (nth a pos-y) pos-x))
        0
        (is-occupied-2 a (+ x pos-x) (+ y pos-y) x y)))))

(defn count-occupied-2 [a pos-x pos-y]
  (+ (is-occupied-2 a (dec pos-x) pos-y -1 0)
     (is-occupied-2 a (inc pos-x) pos-y 1 0)
     (is-occupied-2 a pos-x (inc pos-y) 0 1)
     (is-occupied-2 a (inc pos-x) (inc pos-y) 1 1)
     (is-occupied-2 a (dec pos-x) (inc pos-y) -1 1)
     (is-occupied-2 a pos-x (dec pos-y) 0 -1)
     (is-occupied-2 a (inc pos-x) (dec pos-y) 1 -1)
     (is-occupied-2 a (dec pos-x) (dec pos-y) -1 -1)))

(defn next-step-hull-2 [a pos-x pos-y acc-row acc-all]
  (cond
    (>= pos-x (count (first a)))
    (recur a 0 (inc pos-y) (list) (cons (reverse acc-row) acc-all))

    (>= pos-y (count a)) (reverse acc-all)

    (and (= \L (nth (nth a pos-y) pos-x))
         (zero? (count-occupied-2 a pos-x pos-y)))
    (recur a (inc pos-x) pos-y (cons \# acc-row) acc-all)

    (and (= \# (nth (nth a pos-y) pos-x))
         (>= (count-occupied-2 a pos-x pos-y) 5))
    (recur a (inc pos-x) pos-y (cons \L acc-row) acc-all)

    :else
    (recur a (inc pos-x) pos-y (cons (nth (nth a pos-y) pos-x) acc-row) acc-all)))

(defn next-step-2 [a]
  (next-step-hull-2 a 0 0 (list) (list)))

(defn count-all-steps-2 [step]
  (let [new-step (next-step-2 step)]
    (if (= step new-step)
      (count-occupied-seats step)
      (count-all-steps-2 new-step))))

(defn start-count-all-steps-2 [input]
  (count-all-steps-2 (string/split-lines input)))
