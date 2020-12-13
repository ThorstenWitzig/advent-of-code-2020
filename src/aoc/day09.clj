(ns aoc.day09)

; Hey, I've seen these two functions, they are classics!
; What do you mean you've seen it? It's brand new!
(defn find-number [n lis]
  (first (filter #(= n %) lis)))

(defn find-two-numbers [lis n]
  (if
    (< (count lis) 2)
    nil
    (if-let [sec (find-number (- n (first lis)) (rest lis))]
      (* (first lis) sec)
      (find-two-numbers (rest lis) n))))

;Part 1
(defn find-first-invalid [p lis]
  (if-let [a (find-two-numbers p (first lis))]
    (find-first-invalid (concat (drop 1 p) (list (first lis)))
                        (rest lis))
    (first lis)))

(defn find-first-invalid-with-input [input p-size]
  (find-first-invalid (take p-size input) (drop p-size input)))

;Part 2 (Now this was difficult to make it run in finite time. Hint: Brute force won't work on large inputs)
(defn find-subset-with [n input start end]
  (let [sub-list (subvec input start end)                   ; vec is kinda list, but more like an array. Subvec creates a subvector (its more efficient, which might be useful here..)
        sub-list-sum (reduce + sub-list)]
    (cond
      (= sub-list-sum n) sub-list
      (< sub-list-sum n) (recur n input start (inc end))    ; Recur makes a recursive call, optimized for tail-recursion (see https://en.wikipedia.org/wiki/Tail_call)
      (> sub-list-sum n) (recur n input (inc start) (+ start 2))))) ; Only needed as the jvm doesn't implement it properly :(

(defn calc-encryption-weakness [input]
  (let [subset (-> (find-first-invalid-with-input input 25)
                   (find-subset-with (vec input) 0 1)
                   sort)]
    (+ (first subset)
       (last subset))))
