(ns aoc.day10)

;Part 1
(defn count-num [lis num]
  (->> lis
       (filter #(= % num))
       count))

(defn count-diffs [lis]
  (let [sorted-lis (sort lis)]
    (map #(- %1 %2)           ; Now this part I really like. You know map applies a function to every element of a list?
         (drop 1 sorted-lis) ; Well, in Clojure you could apply it to multiple lists and apply the function to all nth elements of all list. So for (map + (list 1 2) (list 3 4)) you get (list (+ 1 3) (+ 2 4))
         sorted-lis))) ;A Lambda can have more than one argument. In this case, %1 is the first, %2 the second parameter and so on)

(defn count-jolts [i]
  (let [lis (count-diffs i)]
    (* (inc (count-num lis 3))  ;the incs are needed as I left out the first and the 0 and the max+3 Adapter
       (inc (count-num lis 1)))))

; Part 2
; Observation: Only diffs of 1 and three are present in the inputs, the longest list of 1 diffs is 4
; Conclusion: Only obeserve the lists of 1s and use a lazy tribonacci implementation.
(defn count-all-combinations [i]
  (->> (count-diffs i)
       (partition-by #(= 3 %))  ; partition-by groups elements by a predicate, partitioning in this case the list in lists of 1s and 3s
       (remove #(= 3 (first %))) ; remove is btw a nice syntax sugar for (filter (not pred) lis)
       (#(conj (rest %) (conj (first %) 1)))  ;Puts an additional 1 at the first list. Could be avoided if you start with 0 instead
       (map count)
       (map #(cond (= 1 %) 1 ; this is a lazy tribonacci implementation
                   (= 2 %) 2
                   (= 3 %) 4
                   (= 4 %) 7))
       (reduce *)))
