(ns aoc.day17a)

; Part 2
(defn is-active [a pos-x pos-y pos-z pos-f]
  (if (or (< pos-x 0)
          (>= pos-x (count (first (first (first a)))))
          (< pos-y 0)
          (>= pos-y (count (first (first a))))
          (< pos-z 0)
          (>= pos-z (count (first a)))
          (< pos-f 0)
          (>= pos-f (count a)))
    0
    (let [f (nth a pos-f)
          z (nth f pos-z)
          y (nth z pos-y)
          x (nth y pos-x)]
      (if (= \# x)
        1
        0))))

(defn count-occupied [a pos-x pos-y pos-z pos-f]
  (- (->> (map (fn [x]
                 (map (fn [y]
                        (map (fn [z]
                               (map (fn [f]
                                      (is-active a (+ pos-x x) (+ pos-y y) (+ pos-z z) (+ pos-f f)))
                                    [-1 0 1]))
                             [-1 0 1]))
                      [-1 0 1]))
               [-1 0 1])
          flatten
          flatten
          flatten
          (reduce +))
     (is-active a pos-x pos-y pos-z pos-f)))

(defn make-empty-row [n]
  (repeat (+ n 2) \.))

(defn make-empty-surface [x y]
  (repeat (+ y 2) (make-empty-row x)))

(defn make-empty-3d [x y z]
  (repeat (+ z 2) (make-empty-surface x y)))

(defn add-empty-space [a]
  (concat [(make-empty-3d (count (first (first (first a)))) (count (first (first a))) (count (first a)))]
          (map (fn [z]
                 (concat [(make-empty-surface (count (first (first z))) (count (first z)))]
                         (map (fn [x]
                                (concat [(make-empty-row (count x))]
                                        (map (fn [y]
                                               (concat "." y "."))
                                             x)
                                        [(make-empty-row (count x))]))
                              z)
                         [(make-empty-surface (count (first (first z))) (count (first z)))]))
               a)
          [(make-empty-3d (count (first (first (first a)))) (count (first (first a))) (count (first a)))]))

(defn next-step-hull [a pos-x pos-y pos-z pos-f acc-row acc-surface acc-3d acc-all]
  (cond
    (>= pos-x (count (first (first (first a)))))
    (recur a 0 (inc pos-y) pos-z pos-f (list) (cons (reverse acc-row) acc-surface) acc-3d acc-all) ; cons is equivalent to conj, but it guarantees to keep the order. Reverse is needed nevertheless

    (>= pos-y (count (first (first a))))
    (recur a 0 0 (inc pos-z) pos-f (list) (list) (cons (reverse acc-surface) acc-3d) acc-all)

    (>= pos-z (count (first a)))
    (recur a 0 0 0 (inc pos-f) (list) (list) (list) (cons (reverse acc-3d) acc-all))

    (>= pos-f (count a)) (reverse acc-all)

    (and (= \. (nth (nth (nth (nth a pos-f) pos-z) pos-y) pos-x))
         (= 3 (count-occupied a pos-x pos-y pos-z pos-f)))
    (recur a (inc pos-x) pos-y pos-z pos-f (cons \# acc-row) acc-surface acc-3d acc-all)

    (and (= \# (nth (nth (nth (nth a pos-f) pos-z) pos-y) pos-x))
         (or (> (count-occupied a pos-x pos-y pos-z pos-f) 3)
             (< (count-occupied a pos-x pos-y pos-z pos-f) 2)))
    (recur a (inc pos-x) pos-y pos-z pos-f (cons \. acc-row) acc-surface acc-3d acc-all)

    :else
    (recur a (inc pos-x) pos-y pos-z pos-f (cons (nth (nth (nth (nth a pos-f) pos-z) pos-y) pos-x) acc-row) acc-surface acc-3d acc-all)))

(defn next-step [a]
  (println "Step")
  (next-step-hull (add-empty-space a) 0 0 0 0 (list) (list) (list) (list)))



(defn count-occupied-seats [step]
  (->> step
       (map (fn [z]
              (map (fn [x]
                     (map (fn [y]
                            (->> y
                                 (filter (fn [x] (= x \#)))
                                 count))
                          x))
                   z)))
       flatten
       flatten
       (reduce +)))

(defn do-six-times [a]
  (-> a
      next-step
      next-step
      next-step
      next-step
      next-step
      next-step
      ))
