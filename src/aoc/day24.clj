(ns aoc.day24
  (:require [clojure.string :as string]
            [clojure.set :as set]))

;Part 1
(defn direction->coordinates [direction]
  (condp = direction
    "e" {:x 2 :y 0}
    "w" {:x -2 :y 0}
    "se" {:x 1 :y 1}
    "sw" {:x -1 :y 1}
    "ne" {:x 1 :y -1}
    "nw" {:x -1 :y -1}))

(defn add-coordinates [c1 c2]
  {:x (+ (:x c1) (:x c2))
   :y (+ (:y c1) (:y c2))})

(defn read-direction-line [line]
  (if (empty? line)
    line
    (condp = (first line)
      \e (cons "e" (read-direction-line (rest line)))
      \w (cons "w" (read-direction-line (rest line)))
      \s (if (= \e (first (rest line)))
           (cons "se" (read-direction-line (rest (rest line))))
           (cons "sw" (read-direction-line (rest (rest line)))))
      \n (if (= \e (first (rest line)))
           (cons "ne" (read-direction-line (rest (rest line))))
           (cons "nw" (read-direction-line (rest (rest line))))))))

(defn flip-tiles [input]
  (->> input
       string/split-lines
       (map read-direction-line)
       (map #(map direction->coordinates %))
       (map #(reduce add-coordinates %))
       (reduce #(assoc %1 %2 (not (get %1 %2))) {})))

(defn count-black-tiles [tiles]
  (->> tiles
       vals
       (filter identity)
       count))

;Part 2
(def neighbour-coordinates
  [{:x 2 :y 0} {:x -2 :y 0} {:x 1 :y 1} {:x -1 :y -1} {:x -1 :y 1} {:x 1 :y -1}])

; I hope team diversity doesn't look at my naming
(defn count-black-neighbours [dict {x :x y :y}]
  (->> neighbour-coordinates
       (map #(get dict {:x (+ x (:x %)) :y (+ y (:y %))}))
       (filter true?)
       count))

(defn flip-tile [dict coordinates old-dict]
  (let [blm (count-black-neighbours old-dict coordinates)]
    (cond
      (and (get old-dict coordinates)
           (or (= blm 0)
               (> blm 2)))
      (assoc dict coordinates false)

      (and (not (get old-dict coordinates))
           (= blm 2))
      (assoc dict coordinates true)

      :else dict)))

(defn add-two-coordinates [c1 c2]
  {:x (+ (:x c1) (:x c2))
   :y (+ (:y c1) (:y c2))})

(defn get-neighbours-and-self [coordinates]
  (->> neighbour-coordinates
       (map #(add-two-coordinates % coordinates))
       (cons coordinates)))

(defn get-all-tiles-to-look-at [all-coordinates]
  (->> all-coordinates
       (map get-neighbours-and-self)
       flatten
       distinct))

(defn next-day [dict]
  (let [all-tiles (get-all-tiles-to-look-at (keys dict))]
    (reduce #(flip-tile %1 %2 dict) dict all-tiles)))

(defn do-n-days [n dict]
  (if (zero? n)
    dict
    (recur (dec n) (next-day dict))))


