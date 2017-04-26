;;; Touretsky Common Lisp
;;; Chapter 9

;; Exercise 9.1
(println "There are old pilots,
and there are bold pilots,
but there are no old, bold pilots.")

;; Exercise 9.2
(defn draw-line [n]
  (cond (zero? n) (println)
        :else (do (print "*")
                  (recur (- n 1)))))

(draw-line 10)

;; Exercise 9.3
(defn draw-box [cols rows]
  (cond (zero? rows) (print "")
        :else (do (draw-line cols)
                  (recur cols (- rows 1)))))

(draw-box 10 4)

;; Exercise 9.4
(defn ninety-nine-bottles [n]
  (cond (zero? n) (print "")
        :else (do (println (str n " bottles of beer on the wall,
" n " bottles of beer!
Take one down,
Pass it around,
" (- n 1) " bottles of beer on the wall.\n\n"))
                  (recur (- n 1)))))

(ninety-nine-bottles 3)

;; Exercise 9.5
(defn print-board [[p1 p2 p3 p4 p5 p6 p7 p8 p9]]
  (cl-form
