(ns aoc.day23)

(defn find-n-or-highest [n lis-sorted]
  (cond
    (< n (first lis-sorted)) (last lis-sorted)
    (some #{n} lis-sorted) n
    :else (find-n-or-highest (dec n) lis-sorted)))

(defn find-n-or-highest-optimized [n excepts max-number]
  (cond
    (= n 0) max-number
    (some #{n} excepts) (recur (dec n) excepts max-number)
    :else n))

(defn move-cups [maximum [current one two three & remaining]]
  (let [next-cup (find-n-or-highest-optimized (dec current) [one two three] maximum)
        between (take-while #(not= next-cup %) remaining)
        after (rest (drop-while #(not= next-cup %) remaining))]
    (concat between
            [next-cup one two three]
            after
            [current])))

(defn build-linked-dict-step [last dict [x & xs]]
  (if (nil? x)
    dict
    (recur x
           (-> dict
               (assoc last x))
           xs)))

(defn build-linked-dict [lis]
  (build-linked-dict-step (last lis) {} lis))

(defn move-cups-optimized [maximum current dict]
  (let [t1 (get dict current)
        t2 (get dict t1)
        t3 (get dict t2)
        next (find-n-or-highest-optimized (dec current) [t1 t2 t3] maximum)
        new-dict (-> dict
                     (assoc current (get dict t3))
                     (assoc next t1)
                     (assoc t3 (get dict next)))]
    {:next (get dict t3)
     :dict new-dict}))

(defn do-n-moves [moves current maximum dict]
  (if (= moves 0)
    dict
    (let [moved (move-cups-optimized maximum current dict)]
      (recur (dec moves) (:next moved) maximum (:dict moved)))))

(defn add-cups-to-million [cups]
  (concat cups
          (range (inc (count cups)) 1000001)))

(defn print-result [dict]
  (let [after-one (get dict 1)]
    (println after-one)
    (println (get dict after-one))))

(defn do-a-crappy-game [cups]
  (println (System/currentTimeMillis))
  (->> cups
       add-cups-to-million
       build-linked-dict
       (do-n-moves 10000000 (first cups) 1000000)
       print-result
       ((fn [x] (println (System/currentTimeMillis))))))
