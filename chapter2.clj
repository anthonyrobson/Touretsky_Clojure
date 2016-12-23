;;; Touretsky Common Lisp
;;; Chapter 2

(defn my-third1 [xs]
  "Return the third element of list xs; uses first and rest."
  (first (rest (rest xs))))

(= 3 (my-third1 [1 2 3 4])) ; true

(defn my-third2 [xs]
  "Return the third element of list xs; uses second."
  (second (rest xs)))

(= 3 (my-third2 [1 2 3 4])) ; true

(defn two-inputs [a b]
  "Return the sequence (a b)."
  [a b])

(= [1 2] (two-inputs 1 2)) ; true

(defn four-inputs [a b c d]
  "Return the sequence [[a b] [c d]]."
  [[a b] [c d]])

(= [[1 2] [3 4]] (four-inputs 1 2 3 4)) ; true

(defn duo-cons [a b xs]
  "Return the sequence (a b xs), uses cons."
  (cons a (cons b xs)))

(= '(:patrick :seymour :marvin) (duo-cons :patrick :seymour '(:marvin))) ; true

(defn two-deeper1 [x]
  "Return sequence [[x]], using brackets."
  [[x]])

(= [[1]] (two-deeper1 1)) ; true

(defn two-deeper2 [x]
  "Return sequence [[x]], using cons."
  (cons (cons x '()) '()))

(= [[1]] (two-deeper2 1)) ; true

(defn unary-add1 [xs]
  "Add an ':x' to list xs."
  (conj xs :x))

(= [:x :x :x] (unary-add1 [:x :x])) ; true

(defn unary-zero? [xs]
  "Return true if list xs is null."
  (empty? xs))

(= true (unary-zero? [])) ; true

(defn unary-greater? [xs ys]
  "Return true if xs is a longer list than ys."
  (> (count xs) (count ys)))

(= true (unary-greater? [:x :x :x] [:x :x])) ; true
(= false (unary-greater? [:x :x] [:x :x])) ; true
