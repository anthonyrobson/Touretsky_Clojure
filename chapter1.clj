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

(defn two? [n]
  "Return true if n is equal to 2; uses zero? and sub2."
  (zero? (sub2 n)))

(= true (two? 2)) ; true
(= false (two? 3)) ; true

(defn half1 [n]
  "Halve n; defined first way."
  (/ n 2))

(= 5 (half1 10)) ; true

(defn half2 [n]
  "Halve n; defined second way."
  (* n 1/2))

(= 5 (half2 10)) ; true

(defn multi-digit? [n]
  "Return true if n is greater than 9."
  (> n 9))

(= true (multi-digit? 10)) ; true
(= false (multi-digit? 9)) ; true

(defn two-more?1 [x y]
  "Return true if x is two greater than y; uses add2."
  (= x (add2 y)))

(= true (two-more?1 10 8)) ; true
(= false (two-more?1 9 8)) ; true

(defn two-more?2 [x y]
  "Return true if x is two greater than y; uses sub2."
  (= (sub2 x) y))

(= true (two-more?2 10 8)) ; true
(= false (two-more?2 9 8)) ; true

(defn average [x y]
  "Return the arithmetic mean of x and y."
  (/ (+ x y) 2))

(= 9 (average 8 10)) ; true

(defn more-than-half? [x y]
  "Return true if x is more than half of y."
  (> x (/ y 2)))

(= true (more-than-half? 6 10)) ; true
(= false (more-than-half? 5 10)) ; true

(defn not-one? [n]
  "Return true if n is not 1."
  (not= n 1))

(= true (not-one? 2)) ; true
(= false (not-one? 1)) ; true

(defn not-plus? [n]
  "Return true if n is not greater than zero."
  (not (> n 0)))

(= true (not-plus? -1)) ; true
(= false (not-plus? 1)) ; true
