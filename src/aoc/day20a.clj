(ns aoc.day20a
  (:require [clojure.string :as string]))

;Part 2
(def matcher (partial re-matcher #"\d+"))

(defn extract-number [s]
  (-> s
      matcher
      re-find))

(defn to-string [x]
  (reduce str x))

(defn rotate-body [body]
  (->> body
       (map-indexed (fn [xi x]
                      (reverse
                        (map-indexed
                          (fn [yi _]
                            (nth (nth body yi) xi))
                          x))))
       (map to-string)))

(defn rotate [tile]
  {:id     (:id tile)
   :body   (rotate-body (:body tile))
   :top    (to-string (reverse (:left tile)))
   :right  (to-string (:top tile))
   :bottom (to-string (reverse (:right tile)))
   :left   (to-string (:bottom tile))})

(defn flip-body-vertical [body]
  (->> body
       (map reverse)
       (map to-string)))

(defn flip-body-horizontal [body]
  (reverse body))

(defn flip-horizontal [tile]
  {:id     (:id tile)
   :body   (flip-body-horizontal (:body tile))
   :top    (to-string (:bottom tile))
   :bottom (to-string (:top tile))
   :left   (to-string (reverse (:left tile)))
   :right  (to-string (reverse (:right tile)))})



(defn flip-vertical [tile]
  {:id     (:id tile)
   :body   (flip-body-vertical (:body tile))
   :top    (to-string (reverse (:top tile)))
   :bottom (to-string (reverse (:bottom tile)))
   :left   (to-string (:right tile))
   :right  (to-string (:left tile))})

(defn read-tile [input]
  (let [parts (string/split-lines input)]
    {:id     (extract-number (first parts))
     :body   (->> (drop-last parts)
                  (drop 2)
                  (map #(drop 1 (drop-last %)))
                  (map to-string))
     :top    (second parts)
     :bottom (last parts)
     :left   (to-string (map first (drop 1 parts)))
     :right  (to-string (map last (drop 1 parts)))}))

(defn variations-tile [tile]
  [tile
   (rotate tile)
   (rotate (rotate tile))
   (rotate (rotate (rotate tile)))

   (flip-horizontal tile)
   (rotate (flip-horizontal tile))
   (rotate (rotate (flip-horizontal tile)))
   (rotate (rotate (rotate (flip-horizontal tile))))

   (flip-vertical tile)
   (rotate (flip-vertical tile))
   (rotate (rotate (flip-vertical tile)))
   (rotate (rotate (rotate (flip-vertical tile))))])

(defn create-all-tiles-with-variations [input]
  (->> (string/split input #"\n\n")
       (map read-tile)
       (map variations-tile)
       (map distinct)
       flatten))

(defn put-tile-into-map [tile-map tile]
  {:left   (assoc (:left tile-map)
             (:left tile)
             (cons tile (get (:left tile-map) (:left tile))))
   :right  (assoc (:right tile-map)
             (:right tile)
             (cons tile (get (:right tile-map) (:right tile))))
   :top    (assoc (:top tile-map)
             (:top tile)
             (cons tile (get (:top tile-map) (:top tile))))
   :bottom (assoc (:bottom tile-map)
             (:bottom tile)
             (cons tile (get (:bottom tile-map) (:bottom tile))))})

(defn create-tile-map [tiles]
  (reduce put-tile-into-map {} tiles))

(defn find-fitting-tile [current-row acc pos-x pos-y tile-map used-tiles size]
  (cond
    (and (= pos-x size) (= pos-y (dec size)))
    (reverse (cons (reverse current-row) acc))

    (= pos-x size) (recur [] (cons (reverse current-row) acc) 0 (inc pos-y) tile-map used-tiles size)

    (= pos-x 0) (let [bottom-last (:bottom (first (first acc)))
                      fitting-tiles (->> (get (:top tile-map) bottom-last)
                                         (remove #(some #{(:id %)} used-tiles)))]
                  (if (empty? fitting-tiles)
                    nil
                    (->> fitting-tiles
                         (map #(find-fitting-tile (cons % current-row) acc (inc pos-x) pos-y tile-map (cons (:id %) used-tiles) size))
                         (remove nil?)
                         first)))                           ; I assume that there is only one solution. If there are more than one, I won't find the others :(
    (= pos-y 0) (let [right-last (:right (first current-row))
                      fitting-tiles (->> (get (:left tile-map) right-last)
                                         (remove #(some #{(:id %)} used-tiles)))]
                  (if (empty? fitting-tiles)
                    nil
                    (->> fitting-tiles
                         (map #(find-fitting-tile (cons % current-row) acc (inc pos-x) pos-y tile-map (cons (:id %) used-tiles) size))
                         (remove nil?)
                         first)))

    :else (let [right-last (:right (first current-row))
                bottom-last (:bottom (nth (first acc) pos-x))
                fitting-tiles (->> (get (:left tile-map) right-last)
                                   (filter #(= bottom-last (:top %)))
                                   (remove #(some #{(:id %)} used-tiles)))]
            (if (empty? fitting-tiles)
              nil
              (->> fitting-tiles
                   (map #(find-fitting-tile (cons % current-row) acc (inc pos-x) pos-y tile-map (cons (:id %) used-tiles) size))
                   (remove nil?)
                   first)))))

(defn find-square-step [tile-map [tile & tiles] step size]
  (println step)
  (if (nil? tile)
    nil
    (if-let [square (find-fitting-tile [tile] [] 1 0 tile-map [(:id tile)] size)]
      square
      (recur tile-map tiles (inc step) size))))

(defn find-square [input size]
  (let [tiles (create-all-tiles-with-variations input)
        tile-map (create-tile-map tiles)]
    (find-square-step tile-map tiles 0 size)))

(defn merge-two-tiles-body [b1 b2]
  (map #(str %1 %2)
       b1
       b2))

(defn merge-row [list-of-tiles]
  (->> list-of-tiles
       (map :body)                                          ; List of list of strings
       (reduce merge-two-tiles-body)))

(def monster-coordinates
  [[18 0]
   [0 1] [5 1] [6 1] [11 1] [12 1] [17 1] [18 1] [19 1]
   [1 2] [4 2] [7 2] [10 2] [13 2] [16 2]])

(defn is-monster-at [image pos-x pos-y]
  ;(println (str pos-x "," pos-y))
  (every? #(= \# (nth (nth image (+ pos-y (second %))) (+ pos-x (first %))))
          monster-coordinates))

(defn count-monster-step [x y image acc]
  (println (str x "," y "," acc))
  (cond
    (> x (- (count (first image)) 20)) (recur 0 (inc y) image acc)
    (> y (- (count image) 3)) acc
    :else (recur (inc x) y image (if (is-monster-at image x y) (inc acc) acc))))

(defn count-monsters [image]
  (count-monster-step 0 0 image 0))

(defn count-waters [image]
  (->> (reduce str image)
       (filter #(= \# %))
       count))

;square is a list of a list of tiles
(defn merge-to-image [square]
  (->> square
       (map #(merge-row %))
       flatten
       (reduce #(str %1 "\n" %2))))

(defn image-variations [image]
  [image
   (rotate-body image)
   (rotate-body (rotate-body image))
   (rotate-body (rotate-body (rotate-body image)))

   (flip-body-horizontal image)
   (rotate-body (flip-body-horizontal image))
   (rotate-body (rotate-body (flip-body-horizontal image)))
   (rotate-body (rotate-body (rotate-body (flip-body-horizontal image))))

   (flip-body-vertical image)
   (rotate-body (flip-body-vertical image))
   (rotate-body (rotate-body (flip-body-vertical image)))
   (rotate-body (rotate-body (rotate-body (flip-body-vertical image))))])

(defn count-rough-waters [image]
  (->> (string/split-lines image)
       image-variations
       (map #(- (count-waters %) (* 15 (count-monsters %))))))
