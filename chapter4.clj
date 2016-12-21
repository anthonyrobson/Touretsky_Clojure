;;; Touretsky Common Lisp
;;; Chapter 4

(defn make-even [n]
  "Make an odd number n even by adding 1; if already even, just return n."
  (if (even? n)
    n
    (+ n 1)))

(= 4 (make-even 3)) ; true
(= 4 (make-even 4)) ; true

(defn further [n]
  "Make positive n more positive by 1, a negative n more negative by 1, else
  just return n; uses only if."
  (if (zero? n)
    n
    (if (pos? n)
      (+ n 1)
      (- n 1))))

(= 3 (further 2)) ; true
(= -3 (further -2)) ; true
(= 0 (further 0)) ; true

(defn my-not [p]
  "Own definition of not, using only if and constants."
  (if p false true))

(false? (my-not true)) ; true
(true? (my-not false)) ; true

(defn ordered [x y]
  "Return x and y as an ordered collection."
  (if (> y x)
    [x y]
    [y x]))

(= [3 4] (ordered 4 3)) ; true
(= [3 4] (ordered 3 4)) ; true

(defn my-abs [n]
  "Absolute value of n, using cond."
  (cond (zero? n) n
        (pos? n) n
        :else (- n)))

(= 4 (my-abs -4)) ; true
(= 4 (my-abs 4)) ; true
(= 0 (my-abs 0)) ; true

(defn emphasize3 [xs]
  "If the first element of list xs is :good, replace it with :great; if the
  first element is :bad, replace with :awful; otherwise, append :very to the
  beginning of the list."
  (cond (= (first xs) :good) (cons :great (rest xs))
        (= (first xs) :bad) (cons :awful (rest xs))
        :else (cons :very xs)))

(= '(:great :day) (emphasize3 '(:good :day))) ; true
(= '(:awful :day) (emphasize3 '(:bad :day))) ; true
(= '(:very :long :day) (emphasize3 '(:long :day))) ; true

(defn constrain [x min max]
  "If x is less than min, return min; if greater than max, return max; else just
  return x."
  (cond (< x min) min
        (> x max) max
        :else x))

(= 50 (constrain 90 -50 50)) ; true
(= -50 (constrain -90 -50 50)) ; true
(= 5 (constrain 5 -50 50)) ; true

(defn my-cycle [n]
  "If n is 99, return 1; else return n + 1."
  (if (= n 99)
    1
    (+ n 1)))

(= 1 (my-cycle 99)) ; true
(= 2 (my-cycle 1)) ; true

(defn how-compute [x y z]
  "Returns :product-of if z equals the product of x and y; or :sum-of if z
  equals the sum of x and y; else returns :beats-me."
  (cond (= z (* x y)) :product-of
        (= z (+ x y)) :sum-of
        :else :beats-me))

(= :product-of (how-compute 3 4 12)) ; true
(= :sum-of (how-compute 3 4 7)) ; true
(= :beats-me (how-compute 3 4 5)) ; true

(defn geq [x y]
  "Return true if x is greater than or equal to y; not using >=."
  (or (= x y)
      (> x y)))

(true? (geq 10 9)) ; true
(true? (geq 9 9)) ; true
(false? (geq 9 10)) ; true

(defn people [p1 p2]
  "Return true if p1 is either :boy or :girl and p2 is :child; or if p1 is
  either :man or :woman and p2 is :adult."
  (or (and (or (= p1 :boy)
               (= p1 :girl))
           (= p2 :child))
      (and (or (= p1 :man)
               (= p1 :woman))
           (= p2 :adult))))

(true? (people :boy :child)) ; true
(true? (people :man :adult)) ; true
(false? (people :boy :adult)) ; true

(defn rock-paper-scissors [p1 p2]
  "Plays the rock, paper, scissors game."
  (cond (= p1 p2) :tie
        (or (and (= p1 :rock)
                 (= p2 :scissors))
            (and (= p1 :paper)
                 (= p2 :rock))
            (and (= p1 :scissors)
                 (= p2 :paper)))
        :first-wins
        :else :second-wins))

(= :tie (rock-paper-scissors :rock :rock)) ; true
(= :first-wins (rock-paper-scissors :rock :scissors)) ; true
(= :second-wins (rock-paper-scissors :rock :paper)) ; true

(defn boilingp [temp scale]
  "Return true if temp >= 212 and scale is :fahrenheit, or if temp >= 100 and
  scale is :celsius."
  (or (and (>= temp 212)
           (= scale :fahrenheit))
      (and (>= temp 100)
           (= scale :celsius))))

(true? (boilingp 213 :fahrenheit)) ; true
(true? (boilingp 101 :celsius)) ; true
(false? (boilingp 211 :fahrenheit)) ; true
(false? (boilingp 99 :celsius)) ; true
