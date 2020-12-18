(ns aoc.day15)

;Part 1
; Yes, this is kind of unnecessary and using only find-n (which would terminate at the end of the list)
; would have been a better, faster and shorter approach - but I leave it here to document my (strange) thought process
(defn count-n [n lis]
  (count (filter #(= % n) lis)))

(defn find-n [step n [x & xs]]
  (if (= n x)
    step
    (recur (dec step) n xs)))

(defn next-step [end [x & xs :as lis]]                      ; :as defines an alias for the whole list, e.g. lis = (cons x xs)
  (if (= end (count lis))
    x
    (if (= (count-n x xs) 0)
      (recur end (cons 0 lis))
      (recur end (cons (- (count lis)
                          (find-n (count xs) x xs))
                       lis)))))

(defn find-number [end lis]
  (next-step end (reverse lis)))

;Part 2
(defn next-step-optimized [memory step end last-num]
  (if (= end step)
    last-num
    (let [new-mem (assoc memory last-num step)]
      (if-let [m1 (get memory last-num)]
        (recur new-mem (inc step) end (- step m1))
        (recur new-mem (inc step) end 0)))))

(defn find-number-optimized [end lis]
  (let [start (System/currentTimeMillis)
        result (next-step-optimized (->> lis
                                         drop-last
                                         (map-indexed (fn [i x] {x (inc i)}))
                                         (into {}))
                                    (count lis)
                                    end
                                    (last lis))]
    (str "Result: " result ". Found in " (/ (- (System/currentTimeMillis) start) 1000.0) "s")))
