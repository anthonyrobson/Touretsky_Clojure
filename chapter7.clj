;;; Touretsky Common Lisp
;;; Chapter 7

;; Exercise 7.1
(map #(+ 1 %) [1 3 5 7 9]) ; (2 4 6 8 10)

;; Exercise 7.2
(def daily-planet "" [["Olsen" "Jimmy" "123-76-4535" "Cub reporter"]
                      ["Kent"  "Clark" "089-52-6787" "Reporter"]
                      ["Lane"  "Lois"  "951-26-1438" "Reporter"]
                      ["White" "Perry" "355-16-7439" "Editor"]])

(map #(first (rest (rest %))) daily-planet)
; ("123-76-4535" "089-52-6787" "951-26-1438" "355-16-7439")

;; Exercise 7.3
(map zero? [2 0 3 4 0 -5 -6]) ; (false true false false true false false)

;; Exercise 7.4
;; Just write a lambda instead
(map #(> % 5) [1 2 3 4 5 6]) ; (false false false false false true)

;; Exercise 7.5
(#(- % 7) 8) ; 1

;; Exercise 7.6
(#(or (= true %) (= false %)) true) ; true
(#(or (= true %) (= false %)) false) ; true
(#(or (= true %) (= false %)) 5) ; false

;; Exercise 7.7
(defn up-down-flip [xs]
  "Turn every occurrence of `:up` in collection `xs` to `:down` and vice versa."
  (map #(if (= % :up) :down :up) xs))

(up-down-flip [:up :down :up :up]) ; (:down :up :down :down)

;; Exercise 7.8
(defn roughly-equal [xs k]
  "Return the first item in collection `xs` which is roughly equal to `k`,
  'roughly equal' meaning that the item is ±10 within `k`."
  (first (filter #(and (< % (+ k 10)) (> % (- k 10))) xs)))

(roughly-equal [100 200 300 400] 209) ; 200

;; Exercise 7.9
(defn find-nested [xs]
  "Return the first item in collection `xs` which is itself not a collection."
  (first (first (filter list? xs))))

(find-nested '(1 2 (3 4) 5 (6 7) 8)) ; 3

;; Notes

(def note-table "" {"C" 1, "C sharp" 2, "D" 3, "D sharp" 4, "E" 5, "F" 6,
                    "F sharp" 7, "G" 8, "G sharp" 9, "A" 10, "A sharp" 11,
                    "B" 12})

(defn numbers [notes]
  "Transpose the list of `notes` into numbers from map `note-table`."
  (map #(note-table %) notes))

(numbers ["E" "D" "C" "D" "E" "E" "E"]) ; (5 3 1 3 5 5 5)

(defn notes [nums]
  "Transpose the list of `nums` into notes from map `note-table`."
  (let [numbers-table (clojure.set/map-invert note-table)]
    (map #(numbers-table %) nums)))

(notes [5 3 1 3 5 5 5]) ; ("E" "D" "C" "D" "E" "E" "E")

(defn raise [n nums]
  "Raise each item of `nums` by `n`."
  (map #(+ % 5) nums))

(raise 5 [5 3 1 3 5 5 5]) ; (10 8 6 8 10 10 10)

(defn normalize [nums]
  "Normalise list `nums` so that none are below or above 12."
  (map #(cond (> % 12) (- % 12)
              (< % 1) (+ % 12)
              :else %)
       nums))

(normalize [6 10 13]) ; (6 10 1)

(defn transpose [n nts]
  "Transpose the `nts` by `n` and return the notes again."
  (notes (normalize (raise n (numbers nts)))))

(transpose 5 ["E" "D" "C" "D" "E" "E" "E"]) ; ("A" "G" "F" "G" "A" "A" "A")

;; Exercise 7.11
(filter #(and (> % 1) (< % 5)) [1 2 3 4 5 6 7]) ; (2 3 4)

;; Exercise 7.12
(defn count-the [str]
  "Count how often 'the' occurs in `str`."
  (count (filter #(= "the" %) (clojure.string/split str #" "))))

(count-the "the man went to the pub") ; 2

;; Exercise 7.13
(defn pick-twos [xs]
  "Return the list of lists `xs` whose items are two-element lists."
  (filter #(= (count %) 2) (filter coll? xs)))

(pick-twos '(1 2 (3 4) 5 (6 7))) ; ((3 4) (6 7))
(pick-twos [1 2 [3 4] [5] [6 7]]) ; ([3 4] [6 7])

;; Exercise 7.14
(defn my-intersection [xs ys]
  "Define 'intersection' using `filter` or `remove`."
  (filter #(xs %) ys))

(my-intersection #{:a :b :c} #{:a :b :d :e}) ; (:b :a)

;; Cards
;; Instead of lists like (ace spades), I'll use individual strings like
;; "ace spades".

(defn rank [card]
  "Return the rank of `card`."
  (first (clojure.string/split card #" ")))

(rank "ace spades") ; "ace"

(defn suit [card]
  "Return the suit of `card`."
  (second (clojure.string/split card #" ")))

(suit "ace spades") ; "spades"

(def my-hand "" ["3 hearts" "5 clubs" "2 diamonds" "4 diamonds" "ace spades"])

(defn count-suit [s hand]
  "Return the number of cards in `hand` which match suit `s`."
  (count (filter #(= (suit %) s) hand)))

(count-suit "diamonds" my-hand) ; 2

(def colors "" {"clubs"    "black",
                "diamonds" "red",
                "hearts"   "red",
                "spades"   "black"})

(defn color-of [card]
  "Return the colour of card `card` from map `colors`."
  (colors (suit card)))

(color-of "ace spades") ; "black"

(defn first-red [hand]
  "Return the first red card in `hand`."
  (first (filter #(= (color-of %) "red") hand)))

(first-red my-hand) ; "3 hearts"

(defn black-cards [hand]
  "Return only the black cards in hand `hand`."
  (filter #(= (color-of %) "black") hand))

(black-cards my-hand) ; ("5 clubs" "ace spades")

(defn what-ranks [s hand]
  "Return the ranks of cards in `hand` which match suit `s`."
  (map rank (filter #(= (suit %) s) hand)))

(what-ranks "diamonds" my-hand) ; ("2" "4")

(def all-ranks "" ["2" "3" "4" "5" "6" "7" "8" "9" "10" "jack" "queen" "king"
                   "ace"])

(defn higher-rank-p [card1 card2]
  "Return `true` if `card1` has a higher rank than `card2`."
  (> (.indexOf all-ranks (rank card1)) (.indexOf all-ranks (rank card2))))

(higher-rank-p "ace spades" "2 clubs") ; true
(higher-rank-p "2 spades" "3 spades") ; false

(defn high-card [hand]
  "Return the highest-ranked card in `hand`."
  (reduce #(if (higher-rank-p %1 %2) %1 %2) hand))

(high-card my-hand) ; "ace spades"

;; Exercise 7.16
;; union
(reduce clojure.set/union [#{:a :b :c} #{:c :d :a} #{:f :b :d} #{:g}])
; #{:g :c :b :d :f :a}

;; Exercise 7.17
(defn total-lengths [xxs]
  "Return the sum of all lengths of lists in list of lists `xxs`."
  (reduce + (map count xxs)))

(total-lengths [[1 2] [3] [4 5 6]]) ; 6

;; Exercise 7.19
(defn all-odd-p [xs]
  "Return `true` if every item in `xs` is an odd number."
  (every? odd? xs))

(all-odd-p [1 3 5]) ; true
(all-odd-p [1 4 5]) ; false

;; Exercise 7.20
(defn none-odd-p [xs]
  "Return `true` if every item in `xs` is not an odd number."
  (every? #(not (odd? %)) xs))

(none-odd-p [1 4 5]) ; false
(none-odd-p [2 4 6]) ; true

;; Exercise 7.21
(defn not-all-odd-p [xs]
  "Return `true` if not every element of `xs` is odd."
  (not (all-odd-p xs)))

(not-all-odd-p [2 4 6]) ; true
(not-all-odd-p [2 5 6]) ; true
(not-all-odd-p [1 5 7]) ; false

;; Blocks

(def database "" [[:b1 :shape :brick]
                  [:b1 :colour :green]
                  [:b1 :size :small]
                  [:b1 :supported-by :b2]
                  [:b1 :supported-by :b3]
                  [:b2 :shape :brick]
                  [:b2 :colour :red]
                  [:b2 :size :small]
                  [:b2 :supports :b1]
                  [:b2 :left-of :b3]
                  [:b3 :shape :brick]
                  [:b3 :size :small]
                  [:b3 :supports :b1]
                  [:b3 :right-of :b2]
                  [:b4 :shape :pyramid]
                  [:b4 :colour :blue]
                  [:b4 :size :large]
                  [:b4 :supported-by :b5]
                  [:b5 :shape :cube]
                  [:b5 :colour :green]
                  [:b5 :size :large]
                  [:b5 :supports :b4]
                  [:b6 :shape :brick]
                  [:b6 :colour :purple]
                  [:b6 :size :large]])

(def database2 "" {:b1 {:shape :brick, :colour :green, :size :small, :supported-by [:b2 :b3]},
                   :b2 {:shape :brick, :colour :red, :size :small, :supports [:b1], :left-of [:b3]},
                   :b3 {:shape :brick, :size :small, :supports [:b1], :right-of [:b2]},
                   :b4 {:shape :pyramid, :colour :blue, :size :large, :supported-by [:b5]},
                   :b5 {:shape :cube, :colour :green, :size :large, :supports [:b4]},
                   :b6 {:shape :brick, :colour :purple, :size :large}})

(defn match-element [a b]
  "Return `true` if `a` and `b` are equal, or `b` is `:?`."
  (or (= a b)
      (= b :?)))

(match-element :b1 :b1) ; true
(match-element :b1 :?) ; true
(match-element :b1 :b2) ; false

;; FIXME - Massive hack!
(defn match-triple [assertion pattern]
  "Return `true` if the `assertion` matches the `pattern`."
  (every? #(match-element (first %) (second %))
          (partition 2 (interleave assertion pattern))))
                                                           

(match-triple [:b2 :colour :red] [:b2 :colour :?]) ; true
(match-triple [:b2 :colour :red] [:b1 :colour :green]) ; false

(defn fetch [pattern]
  "Return all assertions from `database` which match `pattern`."
  (filter #(match-triple % pattern) database))

(fetch [:b2 :colour :?]) ; ([:b2 :colour :red])
(fetch [:? :supports :b1]) ; ([:b2 :supports :b1] [:b3 :supports :b1])

(fetch [:b4 :shape :?]) ; ([:b4 :shape :pyramid])
(fetch [:? :shape :brick])
; ([:b1 :shape :brick] [:b2 :shape :brick] [:b3 :shape :brick] [:b6 :shape :brick])
(fetch [:b2 :? :b3]) ; ([:b2 :left-of :b3])
(fetch [:? :colour :?])
; ([:b1 :colour :green] [:b2 :colour :red] [:b4 :colour :blue] [:b5 :colour :green] [:b6 :colour :purple])
(fetch [:b4 :? :?])
; ([:b4 :shape :pyramid] [:b4 :colour :blue] [:b4 :size :large] [:b4 :supported-by :b5])

(defn return-colour-pattern [block-name]
  "Return the pattern `[:block-name :colour :?]`."
  [block-name :colour :?])

(return-colour-pattern :b3) ; [:b3 :colour :?]

(defn supporters [block-name]
  "Return all the blocks which support `block-name`."
  (let [pattern [block-name :supported-by :?]]
    (map last (fetch pattern))))

(supporters :b1) ; (:b2 :b3)

(defn cubep [block-name]
  "Return `true` if `block-name` is a cube."
  (let [pattern (first (fetch [block-name :? :?]))]
    (and (= (second pattern) :shape)
         (= (last pattern) :cube))))

(cubep :b5) ; true
(cubep :b4) ; false

(defn supported-by-cube-p [block-name]
  "Return `true` if `block-name` is supported by a cube."
  (some true? (map cubep (supporters block-name))))

(supported-by-cube-p :b4) ; true
(supported-by-cube-p :b1) ; nil

(defn desc1 [block-name]
  "Return all assertions regarding `block-name`."
  (map rest (fetch [block-name :? :?])))

(desc1 :b6) ; ((:shape :brick) (:colour :purple) (:size :large))

(defn description [block-name]
  "Return all assertions about `block-name` as a single list."
  (reduce concat (desc1 block-name)))

(description :b6) ; (:shape :brick :colour :purple :size :large)
(description :b1) ; (:shape :brick :colour :green :size :small :supported-by :b2 :supported-by :b3)
(description :b4) ; (:shape :pyramid :colour :blue :size :large :supported-by :b5)
