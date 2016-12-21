;;; Touretsky Common Lisp
;;; Chapter 3

(defn half [n]
  "Halve input n."
  (/ n 2))

(= 5 (half 10)) ; true

(defn cube [n]
  "Cube input n."
  (* n n n))

(= 27 (cube 3)) ; true

(defn one-more-p [x y]
  "Return true if x is one greater than y."
  (= (- x 1) y))

(true? (one-more-p 10 9)) ; true

(defn pythag [x y]
  "Return square root of x^2 + y^2."
  (Math/sqrt (+ (* x x) (* y y))))

(= 5.0 (pythag 3 4)) ; true

(defn longer-than [xs ys]
  "Return true if xs is a longer list than ys."
  (> (count xs) (count ys)))

(true? (longer-than [1 2 3] [1 2])) ; true

(defn add-length [xs]
  "Prepend length of list xs to xs."
  (conj xs (count xs)))

(= '(4 moo goo gai pan) (add-length '(moo goo gai pan))) ; true

(defn my-fun [a b]
  "Return the collection [[a] b]."
  [[a] b])

(= [[1] 2] (my-fun 1 2)) ; true

(defn firstp [x ys]
  "Return true if x is the first element in list ys."
  (= x (first ys)))

(true? (firstp :foo '(:foo :bar :baz))) ; true
(false? (firstp :boing '(:foo :bar :baz))) ; true

(defn mid-add1 [xs]
  "Add 1 to the middle element of three-element list xs."
  (list (first xs) (+ 1 (second xs)) (nth xs 2)))

(= '(take 3 cookies) (mid-add1 '(take 2 cookies))) ; true

(defn f-to-c [temp]
  "Convert Fahrenheit temp to Celsius."
  (/ (* (- temp 32) 5) 9))

(= 0 (f-to-c 32)) ; true
