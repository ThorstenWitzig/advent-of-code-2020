(ns aoc.day20
  (:require [clojure.string :as string]))

; Part 1
(def matcher (partial re-matcher #"\d+"))

(defn extract-number [s]
  (-> s
      matcher
      re-find))

(defn to-string [x]
  (reduce str x))

(defn rotate [tile]
  {:id     (:id tile)
   :top    (to-string (reverse (:left tile)))
   :right  (to-string (:top tile))
   :bottom (to-string (reverse (:right tile)))
   :left   (to-string (:bottom tile))})

(defn flip-horizontal [tile]
  {:id     (:id tile)
   :top    (to-string (:bottom tile))
   :bottom (to-string (:top tile))
   :left   (to-string (reverse (:left tile)))
   :right  (to-string (reverse (:right tile)))})

(defn flip-vertical [tile]
  {:id     (:id tile)
   :top    (to-string (reverse (:top tile)))
   :bottom (to-string (reverse (:bottom tile)))
   :left   (to-string (:right tile))
   :right  (to-string (:left tile))})

(defn read-tile [input]
  (let [parts (string/split-lines input)]
    {:id     (extract-number (first parts))
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
  (println (str pos-x "," pos-y "," (:id (first current-row))))
  (cond
    (and (= pos-x size) (= pos-y (dec size)))
    (reverse (cons current-row acc))

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
  (println step "," (:id tile))
  (if (nil? tile)
    nil
    (if-let [square (find-fitting-tile [tile] [] 1 0 tile-map [(:id tile)] size)]
      square
      (recur tile-map tiles (inc step) size))))

(defn find-square [input size]
  (let [tiles (create-all-tiles-with-variations input)
        tile-map (create-tile-map tiles)]
    (find-square-step tile-map tiles 0 size)))
