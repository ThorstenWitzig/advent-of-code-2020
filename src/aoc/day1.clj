(ns aoc.day1)

;Part one
; defn defines a new function. [n lis] defines a list of variables (two in this case). Can be empty
(defn find-number [n lis]
  (->> lis                  ; ->> is a nice macro to avoid to stack call into another. It puts the result of line n as the last argument into line n+1.
       (filter #(= n %))    ; e.g. this function would be written as (first (filter #(= n %) lis))
       first))                                              ; #(= n %) is a lambda and the short version of (fn [x] (= n x))


(defn find-two-numbers [n lis]
  (if
    (>= (count lis) 2)
    (if-let [sec (find-number (- n (first lis)) (rest lis))] ; if-let puts a value into variable (sec here) and
      (* (first lis) sec)   ; evaluates the first part if its not nil, the second otherwise. (nil is somehow equivalent to null in java etc.)
      (find-two-numbers n (rest lis)))))

; Part 2
; Just the what you already got
(defn find-three-numbers [n lis]
  (if (>= (count lis) 3)
    (if-let [sec (find-two-numbers (- n (first lis)) (rest lis))] ;btw, first gives the first element of a list (no shit,sherlock!) and rest the remaining elements in the list. There is also seconds, might be handy some day
      (* (first lis) sec)
      (find-three-numbers n (rest lis)))))
