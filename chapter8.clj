;;; Touretsky Common Lisp
;;; Chapter 8 - Recursion

(defn any-odd?
  "Return true if any element in list XS is an odd number."
  [xs]
  (cond (empty? xs) false
        (odd? (first xs)) true
        :else (any-odd? (rest xs))))

(any-odd? [2 4 6 7]) ; true
(any-odd? [2 4 6 8]) ; false

(defn any-odd-2?
  "Return true if any element in list XS is an odd number."
  [xs]
  (cond (empty? xs) false
        (odd? (first xs)) true
        :else (recur (rest xs))))

(any-odd-2? [2 4 6 7]) ; true
(any-odd-2? [2 4 6 8]) ; false

(defn laugh
  "Return a coll of n 'ha'."
  [n]
  (cond (zero? n) ()
        :else (cons 'ha (laugh (- n 1)))))

(laugh 4) ; (ha ha ha ha)

(defn add-up
  "Sum the numbers in a list."
  [xs]
  (cond (empty? xs) 0
        :else (+ (first xs) (add-up (rest xs)))))

(add-up [2 3 7]) ; 12

(defn add-up-2
  "Sums the numbers in a list."
  [xs]
  (loop [sum 0 xs xs]
    (if (empty? xs)
      sum
      (recur (+ sum (first xs)) (rest xs)))))

(add-up-2 [2 3 7]) ; 12

(defn all-odd?
  "Return true if all elements in list are odd numbers."
  [xs]
  (cond (empty? xs) true
        :else (and (odd? (first xs))
                   (all-odd? (rest xs)))))

(all-odd? [1 3 5]) ; true
(all-odd? [1 4 5]) ; false

(defn my-member
  "If X is an element in list XS, return the CDR which begins with X."
  [x xs]
  (cond (empty? xs) ()
        (= x (first xs)) xs
        :else (my-member x (rest xs))))

(my-member 3 [1 2 3 4 5]) ; (3 4 5)

(defn my-assoc
  "Return a sublist within XXS whose first element matches X."
  [x xxs]
  (cond (empty? xxs) ()
        (= x (first (first xxs))) (first xxs)
        :else (my-assoc x (rest xxs))))

(my-assoc 3 {1 "one", 2 "two", 3 "three"}) ; [3 "three"]

(defn my-nth
  "Return the Nth element of list XS."
  [n xs]
  (cond (empty? xs) ()
        (zero? n) (first xs)
        :else (my-nth (- n 1) (rest xs))))

(my-nth 2 [1 2 3 4 5]) ; 3

(defn rec-plus
  "Recursively add X to Y."
  [x y]
  (if (zero? x)
    y
    (recur (- x 1) (+ y 1))))

(rec-plus 10 10) ; 20

(defn fib
  "Return the Nth Fibonacci number."
  [n]
  (cond (= n 0) 1
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

(fib 4) ; 5
(fib 5) ; 8

(defn any-7?
  "Return true if any element in list XS is 7."
  [xs]
  (cond (empty? xs) false
        (= (first xs) 7) true
        :else (any-7? (rest xs))))

(any-7? [4 5 6]) ; false
(any-7? [4 5 6 7]) ; true

(defn find-first-odd
  "Return the first odd number in list XS."
  [xs]
  (cond (empty? xs) nil
        (odd? (first xs)) (first xs)
        :else (find-first-odd (rest xs))))

(find-first-odd [2 4 6 7 8]) ; 7

(defn add-nums
  "Adds N, N-1, N-2... 0."
  [n]
  (cond (zero? n) n
        :else (+ n
                 (add-nums (- n 1)))))

(add-nums 5) ; 15

(defn count-down
  "Create a list of N, N-1, N-2... 1."
  [n]
  (cond (zero? n) ()
        :else (cons n (count-down (- n 1)))))

(count-down 5) ; (5 4 3 2 1)

(defn square-list
  "Create a list which is made of the square of each element of XS."
  [xs]
  (cond (empty? xs) ()
        :else (cons (* (first xs) (first xs))
                    (square-list (rest xs)))))

(square-list [3 4 5 6]) ; (9 16 25 36)

(defn my-nth
  "Return the Nth element of list XS."
  [n xs]
  (cond (zero? n) (first xs)
        :else (my-nth (- n 1) (rest xs))))

(my-nth 2 [1 2 3 4]) ; 3

;; Recursion templates

;; Double-test tail recursion

;; (defun f (x)
;;   (cond (end-test-1 end-value-1)
;;         (end-test-2 end-value-2)
;;         (t (f reduced-x))))

;; Single-test tail recursion
;; (when one knows that the function will always find what it seeks)

;; (defun f (x)
;;   (cond (end-test end-value)
;;   (t (f reduced-x))))

;; Augmenting recursion
;; (builds up some result)

;; (defun f (x)
;;   (cond (end-test end-value)
;;         (t (augmenting-function augmenting-value
;;              (f reduced-x)))))

;; List-consing recursion
;; (a type of augmenting recursion; creates a list)

;; (defun f (n)
;;   (cond (end-test nil)
;;         (t (cons new-element
;;                  (f reduced-n)))))

;; Simultaneous recursion on several variables
;; (a type of single-test tail recursion)

;; (defun f (n x)
;;   (cond (end-test end-value)
;;         (t (f reduced-n reduced-x))))

;; Conditional augmentation

;; (defun f (x)
;;   (cond (end-test end-value)
;;         (aug-test (aug-fun aug-val (f reduced-x)))
;;         (t (f reduced-x))))

(defn sum-numeric-elements
  "Sum all numeric elements in list XS."
  [xs]
  (cond (empty? xs) 0
        (number? (first xs)) (+ (first xs) (sum-numeric-elements (rest xs)))
        :else (sum-numeric-elements (rest xs))))

(sum-numeric-elements '(3 bears 3 bowls and 1 girl)) ; 7

(defn my-remove
  "Remove all elements matching X in list XS."
  [x xs]
  (cond (empty? xs) ()
        (= x (first xs)) (my-remove x (rest xs))
        :else (cons (first xs) (my-remove x (rest xs)))))

(my-remove 3 [1 2 3 4 5 3]) ; (1 2 4 5)

(defn my-intersection
  "Recursive definition of intersection."
  [xs ys]
  (cond (empty? xs) ()
        (some #(= (first xs) %) ys) (cons (first xs) (my-intersection (rest xs) ys))
        :else (my-intersection (rest xs) ys)))

(my-intersection [1 2 3] [3 4 5]) ; (3)

(defn count-odd
  "Count the number of odd numbers in list XS."
  [xs]
  (cond (empty? xs) 0
        (odd? (first xs)) (+ 1 (count-odd (rest xs)))
        :else (count-odd (rest xs))))

(count-odd [4 5 6 7 8]) ; 2

;; Multiple recursion
;; (makes more than one recursive call each invocation)

;; (defun f (n)
;;   (cond (end-test-1 end-value-1)
;;         (end-test-2 end-value-2)
;;         (t (combiner (f first-reduced-n)
;;                      (f second-reduced-n)))))

;; CAR/CDR recursion
;; (special case of multiple recursion)

;; (defun f (x)
;;   (cond (end-test-1 end-value-1)
;;         (end-test-2 end-value-2)
;;         (t (combiner (f (car x))
;;                      (f (cdr x))))))

;; FIXME
(defn sum-tree
  "Sum all numbers in a tree XS."
  [xs]
  (cond (number? xs) xs
        (seq? xs) 0
        :else (+ (sum-tree (first xs))
                 (sum-tree (rest xs)))))

(sum-tree [[3 :bears] [3 :bowls] [1 :girl]]) ; 3

;; FIXME
;; Clojure has already 'flatten'
(defn my-flatten
  "Flatten a nested list into one list."
  [xs]
  (cond (not (seq? xs)) (list xs)
        :else (concat (my-flatten (first xs))
                      (and (rest xs) (my-flatten (rest xs))))))

(my-flatten [[1] [2] [3]])

;; Exercise 8.47
(defn make-loaf
  [n]
  (if (zero? n)
    ()
    (cons :x (make-loaf (- n 1)))))

(make-loaf 4) ; (:x :x :x :x)

;; Exercise 8.48
(defn bury
  [x n]
  (cond (zero? n) x
        :else (bury (list x) (- n 1))))

(bury "fred" 2) ; (("fred"))
(bury "fred" 5) ; ((((("fred")))))

;; Exercie 8.49
(defn pairings
  [xs ys]
  (cond (empty? xs) ()
        :else (cons (list (first xs) (first ys))
                    (pairings (rest xs) (rest ys)))))

(pairings '(a b c) '(1 2 3)) ; ((a 1) (b 2) (c 3))

;;; MINI-KEYBOARD EXERCISE

(def family
  [["Colin"     nil       nil]
   ["Deirdre"   nil       nil]
   ["Arthur"    nil       nil]
   ["Kate"      nil       nil]
   ["Frank"     nil       nil]
   ["Linda"     nil       nil]
   ["Suzanne"   "Colin"   "Deirdre"]
   ["Bruce"     "Arthur"  "Kate"]
   ["Charles"   "Arthur"  "Kate"]
   ["David"     "Arthur"  "Kate"]
   ["Ellen"     "Arthur"  "Kate"]
   ["George"    "Frank"   "Linda"]
   ["Hillary"   "Frank"   "Linda"]
   ["Andre"     nil       nil]
   ["Tamara"    "Bruce"   "Suzanne"]
   ["Vincent"   "Bruce"   "Suzanne"]
   ["Wanda"     nil       nil]
   ["Ivan"      "George"  "Ellen"]
   ["Julie"     "George"  "Ellen"]
   ["Marie"     "George"  "Ellen"]
   ["Nigel"     "Andre"   "Hillary"]
   ["Frederick" nil       "Tamara"]
   ["Zelda"     "Vincent" "Wanda"]
   ["Joshua"    "Ivan"    "Wanda"]
   ["Quentin"   nil       nil]
   ["Robert"    "Quentin" "Julie"]
   ["Olivia"    "Nigel"   "Marie"]
   ["Peter"     "Nigel"   "Marie"]
   ["Erica"     nil       nil]
   ["Yvette"    "Robert"  "Zelda"]
   ["Diane"     "Peter"   "Erica"]])

(defn father
  [person]
  (loop [db family]
    (cond (empty? db) nil
          (= person (ffirst db)) (nth (first db) 1)
          :else (recur (rest db)))))

(father "Diane") ; "Peter"
(father "Robert") ; "Quentin"

(defn mother
  [person]
  (loop [db family]
    (cond (empty? db) nil
          (= person (ffirst db)) (nth (first db) 2)
          :else (recur (rest db)))))

(mother "Diane") ; "Erica"
(mother "Yvette") ; "Zelda"

(defn parentes
  [person]
  (list (father person) (mother person)))

(parentes "Diane") ; ("Peter" "Erica")
(parentes "Yvette") ; ("Robert" "Zelda")
(parentes "Frederick") ; (nil "Tamara")

(defn infantes
  [person]
  (loop [db family output ()]
    (cond (empty? db) output
          (or (= person (nth (first db) 1))
              (= person (nth (first db) 2)))
          (recur (rest db) (conj output (ffirst db)))
          :else (recur (rest db) output))))

(infantes "Arthur") ; ("Ellen" "David" "Charles" "Bruce")
          
(defn siblings
  [person]
  (set (remove #(= % person)
               (reduce #'concat
                       (map #'infantes
                            (parentes person))))))

(siblings "Bruce") ; #{"Ellen" "Charles" "David"}

(defn mapunion
  [f xs]
  (set (reduce #'concat (map f xs))))

(mapunion #'rest '((1 a b c) (2 e c j) (3 f a b c d)))

(defn grandparents
  [person]
  (mapunion #'parentes (parentes person)))

(grandparents "Yvette") ; #{"Wanda" "Julie" "Quentin" "Vincent"}

(defn cousins
  [person]
  (mapunion #'infantes (mapunion #'siblings (parentes person))))

(cousins "Julie") ; #{"Nigel" "Tamara" "Vincent"}

(defn descended-from?
  [name descendent]
  (cond (nil? name) false
        (nil? descendent) false
        (some #{descendent} (parentes name)) true
        :else (or (descended-from? (father name) descendent)
                  (descended-from? (mother name) descendent))))

(descended-from? "Tamara" "Arthur") ; true
(descended-from? "Tamara" "Linda") ; false

(defn ancestores
  [person]
  (cond (nil? person) nil
        :else (remove #'nil?
        (clojure.set/union (parentes person)
           (clojure.set/union (ancestores (father person))
                              (ancestores (mother person)))))))
  
(ancestores "Marie") ; ("Ellen" "George" "Kate" "Arthur" "Frank" "Linda")

;; FIXME!
(defn gen-gap-recursive
  [name ancestor]
  (cond (nil? (descended-from? name ancestor)) 0
        (some #{ancestor} (parentes name)) 1
        :else (+ 1
                 (gen-gap-recursive (father name) ancestor)
                 (gen-gap-recursive (mother name) ancestor))))

(defn generation-gap
  [name ancestor]
  (cond (nil? name) nil
        (nil? ancestor) nil
        (descended-from? name ancestor) (gen-gap-recursive name ancestor)
        :else nil))

(generation-gap "Suzanne" "Colin") ; 1
(generation-gap "Frederick" "Colin") ; 
