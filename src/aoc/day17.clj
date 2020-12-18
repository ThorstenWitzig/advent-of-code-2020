(ns aoc.day17)

; Part 1
(defn is-active [a pos-x pos-y pos-z]
  (if (or (< pos-x 0)
          (>= pos-x (count (first (first a))))
          (< pos-y 0)
          (>= pos-y (count (first a)))
          (< pos-z 0)
          (>= pos-z (count a)))
    0
    (if (= \# (nth (nth (nth a pos-z) pos-y) pos-x))
      1
      0)))

(defn count-occupied [a pos-x pos-y pos-z]
  (- (->> (map (fn [x]
                 (map (fn [y]
                        (map (fn [z]
                               (is-active a (+ pos-x x) (+ pos-y y) (+ pos-z z)))
                             [-1 0 1]))
                      [-1 0 1]))
               [-1 0 1])
          flatten
          flatten
          (reduce +))
     (is-active a pos-x pos-y pos-z)))

(defn make-empty-row [n]
  (repeat (+ n 2) \.))

(defn make-empty-surface [x y]
  (repeat (+ y 2) (make-empty-row x)))

(defn add-empty-space [a]
  (concat [(make-empty-surface (count (first (first a))) (count (first a)))]
          (map (fn [x]
                 (concat [(make-empty-row (count x))]
                         (map (fn [y]
                                (concat "." y "."))
                              x)
                         [(make-empty-row (count x))]))
               a)
          [(make-empty-surface (count (first (first a))) (count (first a)))]))

(defn next-step-hull [a pos-x pos-y pos-z acc-row acc-surface acc-all]
  (cond
    (>= pos-x (count (first (first a))))
    (recur a 0 (inc pos-y) pos-z (list) (cons (reverse acc-row) acc-surface) acc-all) ; cons is equivalent to conj, but it guarantees to keep the order. Reverse is needed nevertheless

    (>= pos-y (count (first a)))
    (recur a 0 0 (inc pos-z) (list) (list) (cons (reverse acc-surface) acc-all))

    (>= pos-z (count a)) (reverse acc-all)

    (and (= \. (nth (nth (nth a pos-z) pos-y) pos-x))
         (= 3 (count-occupied a pos-x pos-y pos-z)))
    (recur a (inc pos-x) pos-y pos-z (cons \# acc-row) acc-surface acc-all)

    (and (= \# (nth (nth (nth a pos-z) pos-y) pos-x))
         (or (> (count-occupied a pos-x pos-y pos-z) 3)
             (< (count-occupied a pos-x pos-y pos-z) 2)))
    (recur a (inc pos-x) pos-y pos-z (cons \. acc-row) acc-surface acc-all)

    :else
    (recur a (inc pos-x) pos-y pos-z (cons (nth (nth (nth a pos-z) pos-y) pos-x) acc-row) acc-surface acc-all)))

(defn next-step [a]
  (next-step-hull (add-empty-space a) 0 0 0 (list) (list) (list)))



(defn count-occupied-seats [step]
  (->> step
       (map (fn [x]
              (map (fn [y]
                     (->> y
                          (filter (fn [x] (= x \#)))
                          count))
                   x)))
       (flatten)
       (reduce +)))

(defn do-six-times [a]
  (-> a
      next-step
      next-step
      next-step
      next-step
      next-step
      next-step
      count-occupied-seats))
