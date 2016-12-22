;;; Touretsky Common Lisp
;;; Chapter 5

(defn throw-die []
  "Simulate the throwing of a single die, returning a random integer between
  1 and 6."
  (+ 1 (rand-int 5)))

(defn throw-dice []
  "Simulate the throwing of two dice, returning a vector of two random integers,
  using `throw-die`."
  [(throw-die) (throw-die)])

(defn snake-eyes-p [th]
  "Return `true` if the throw `th` is (1 1)."
  (and (= (first th) 1)
       (= (second th) 1)))

(defn boxcars-p [th]
  "Return `true` if the throw `th` is (6 6)."
  (and (= (first th) 6)
       (= (second th) 6)))

(defn throw-sum [th]
  "Returns the sum of a throw `th`."
  (+ (first th) (second th)))

(defn instant-win-p [th]
  "Return `true` if the sum of throw `th` is 7 or 11."
  (or (= (throw-sum th) 7)
      (= (throw-sum th) 11)))

(defn instant-loss-p [th]
  "Return `true` if the sum of throw `th` is 2, 3, or 12."
  (or (= (throw-sum th) 2)
      (= (throw-sum th) 3)
      (= (throw-sum th) 12)))

(defn say-throw [th]
  "Return 'snake-eyes' if the throw `th` is (1 1) or 'boxcars' if it is (6 6),
  else return the sum of the throw."
  (cond (snake-eyes-p th) "snake-eyes"
        (boxcars-p th) "boxcars"
        :else (throw-sum th)))

(defn craps []
  "Play a game of craps, outputting a suitable string to inform player of the
  outcome."
  (let [th (throw-dice)
        str1 (str "Throw " (first th) " and " (second th) " -- ")]
    (cond (instant-win-p th) (str str1 (say-throw th) " -- you win!")
          (instant-loss-p th) (str str1 (say-throw th) " -- you lose!")
          :else (str str1 "your point is " (throw-sum th)))))

(defn try-for-point [point]
  "Throw again and if `point` is the sum, the game is won; if 7 is the sum,
  the game is lost; else output the throw sum to the player."
  (let [th (throw-dice)
        str1 (str "Throw " (first th) " and " (second th) " -- ")]
    (cond (= point (throw-sum th)) (str str1 (throw-sum th) " -- you win!")
          (= (throw-sum th) 7) (str str1 (throw-sum th) " -- you lose!")
          :else (str str1 "your point is " (throw-sum th)))))
