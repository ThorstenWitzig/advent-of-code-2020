(ns aoc.day1)

(defn find-number [n lis]
  (first (filter #(= n %) lis)))

(defn find-two-numbers [lis n]
  (if
    (< (count lis) 2)
    nil
    (if-let [sec (find-number (- n (first lis)) (rest lis))]
      (* (first lis) sec)
      (find-two-numbers (rest lis) n))))

(defn find-three-numbers [lis n]
  (if (< (count lis) 3)
    nil
    (if-let [sec (find-two-numbers (rest lis) (- n (first lis)))]
      (* (first lis) sec)
      (find-three-numbers (rest lis) n))))
