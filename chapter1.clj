;;; Touretsky's Common Lisp
;;; Chapter 1

(defn add1 [n]
  "Add 1 to input n."
  (+ n 1))

(= 2 (add1 1)) ; true

(defn add2 [n]
  "Add 2 to input n."
  (+ n 2))

(= 3 (add2 1)) ; true

(defn sub2 [n]
  "Subtract 2 from input n."
  (- n 2))

(= 3 (sub2 5)) ; true

(defn two-p [n]
  "Return true if n is equal to 2; uses zero? and sub2."
  (zero? (sub2 n)))

(= true (two-p 2)) ; true
(= false (two-p 3)) ; true

(defn half1 [n]
  "Halve n; defined first way."
  (/ n 2))

(= 5 (half1 10)) ; true

(defn half2 [n]
  "Halve n; defined second way."
  (* n 1/2))

(= 5 (half2 10)) ; true

(defn multi-digit-p [n]
  "Return true if n is greater than 9."
  (> n 9))

(= true (multi-digit-p 10)) ; true
(= false (multi-digit-p 9)) ; true

(defn two-more-p1 [x y]
  "Return true if x is two greater than y; uses add2."
  (= x (add2 y)))

(= true (two-more-p1 10 8)) ; true
(= false (two-more-p1 9 8)) ; true

(defn two-more-p2 [x y]
  "Return true if x is two greater than y; uses sub2."
  (= (sub2 x) y))

(= true (two-more-p2 10 8)) ; true
(= false (two-more-p2 9 8)) ; true

(defn average [x y]
  "Return the arithmetic mean of x and y."
  (/ (+ x y) 2))

(= 9 (average 8 10)) ; true

(defn more-than-half-p [x y]
  "Return true if x is more than half of y."
  (> x (/ y 2)))

(= true (more-than-half-p 6 10)) ; true
(= false (more-than-half-p 5 10)) ; true

(defn not-one-p [n]
  "Return true if n is not 1."
  (not= n 1))

(= true (not-one-p 2)) ; true
(= false (not-one-p 1)) ; true

(defn not-plus-p [n]
  "Return true if n is not greater than zero."
  (not (> n 0)))

(= true (not-plus-p -1)) ; true
(= false (not-plus-p 1)) ; true


