;;; Touretsky Common Lisp
;;; Chapter 6

(defn last-element1 [xs]
  "Return the last item of collection `xs` using the function `last`. (Clojure's
  `last` automatically returns the 'car' of the last item, so this is very/too
  simple.)"
  (last xs))

(defn last-element2 [xs]
  "Return the last item of collection `xs` using the function `reverse`."
  (first (reverse xs)))

(defn last-element3 [xs]
  "Return the last item of collection `xs` using the functions `nth` and
  `count`."
  (nth xs (- (count xs) 1)))

(defn next-to-last1 [xs]
  "Return the penultimate item of collection `xs` using the function `reverse`."
  (second (reverse xs)))

(defn next-to-last2 [xs]
  "Return the penultimate item of collection `xs` using the function `nth`."
  (nth xs (- (count xs) 2)))

(defn my-but-last1 [xs]
  "Returns the collection `xs` minus its last element. Clojure has the `butlast`
  function for this purpose."
  (reverse (rest (reverse xs))))

(defn my-but-last2 [xs]
  "Show how the `butlast` function works."
  (butlast xs))

(defn palindrome? [xs]
  "Return `true` if the collection `xs` is palindromic."
  (= xs (reverse xs)))

(defn make-palindrome [xs]
  "Make collection `xs` palindromic by appending the reverse of it onto its
  start/end."
  (concat xs (reverse xs)))

;;; Sets

(defn contains-article? [xs]
  "Return `true` if collection `xs` contains any ':a', ':an', or ':the'."
  (seq (clojure.set/intersection #{:a :an :the} (set xs))))

(defn add-vowels [xs]
  "Take a collection `xs` and add the set of vowels #{:a :e :i :o :u} to it."
  (clojure.set/union #{:a :e :i :o :u} (set xs)))

(defn my-subset? [xs ys]
  "Return `true` if `xs` is a subset of `ys`."
  (empty? (clojure.set/difference (set xs) (set ys))))

(defn set-equal? [xs ys]
  "Return `true` if `xs` is 'set-equal' to `ys`."
  (and (my-subset? xs ys)
       (my-subset? ys xs)))

(defn proper-subset? [xs ys]
  "Return `true` if `xs` is a proper subset of `ys`, that is, if `xs` is a
  subset of `ys`, but is not 'set-equal' to it."
  (and (my-subset? xs ys)
       (not (set-equal? xs ys))))

;;; Shape features

(def test-string "" "large red shiny cube -vs- small shiny red four-sided pyramid")

(defn right-side [xs]
  "Return the right-hand side of a 'features vector'."
  (second (clojure.string/split xs #" -vs- ")))

(= "small shiny red four-sided pyramid" (right-side test-string))

(defn left-side [xs]
  "Return the left-hand side of a 'features vector'."
  (first (clojure.string/split xs #" -vs- ")))

(= "large red shiny cube" (left-side test-string))

(defn count-common [xs]
  "Return how many common features the left and right sides of a description
  `xs` have."
  (count (clojure.set/intersection (set (clojure.string/split (left-side xs) #" "))
                                   (set (clojure.string/split (right-side xs) #" ")))))

(= 2 (count-common test-string))

(defn compare [xs]
  "Pretty-print an output describing how many common features the left and right
  sides of a description `xs` have in common."
  (str (count-common xs) " common features"))

(def books "" {"War and Peace" "Leo Tolstoy",
               "1984" "George Orwell",
               "Brave New World" "Aldous Huxley",
               "Das Kapital" "Karl Marx",
               "Animal Farm" "George Orwell"})

(defn who-wrote [book]
  "Return the author associated with `book` in `books`."
  (books book))

(def nerd-states "" {"sleeping" "eating",
                     "eating" "waiting for a computer",
                     "waiting for a computer" "programming",
                     "programming" "debugging",
                     "debugging" "sleeping"})

(defn nerdus [state]
  "Return the 'nerd state' which occurs next in `nerd-states`."
  (nerd-states state))

(defn sleepless-nerd [state]
  "Miss `sleeping` from `nerd-states`: if inputted `state` is `debugging`,
  skip straight to `eating`."
  (if (= state "debugging")
    "eating"
    (nerdus state)))

(defn nerd-on-caffeine [state]
  "Skip ahead two states in `nerd-states`."
  (nerdus (nerdus state)))

(defn swap-first-last [xs]
  "Return the collection `xs` with its first and last items swapped."
  (flatten (list (last xs) (rest (butlast xs)) (first xs))))

(defn rotate-left [xs]
  "Return collection `xs` so that (a b c d e) becomes (b c d e a)."
  (concat (rest xs) (cons (first xs) '())))

(defn rotate-right [xs]
  "Return collection `xs` so that (a b c d e) becomes (e a b c d)."
  (concat (cons (last xs) '()) (butlast xs)))

;;; Robbie

(def rooms "" {:living-room {:north :front-stairs,
                             :south :dining-room,
                             :east :kitchen},
               :upstairs-bedroom {:west :library,
                                  :south :front-stairs},
               :dining-room {:north :living-room,
                             :east :pantry,
                             :west :downstairs-bedroom},
               :kitchen {:west :living-room,
                         :south :pantry},
               :pantry {:north :kitchen,
                        :west :dining-room},
               :downstairs-bedroom {:north :back-stairs,
                                    :east :dining-room},
               :back-stairs {:south :downstairs-bedroom,
                             :north :library},
               :front-stairs {:north :upstairs-bedroom,
                              :south :living-room},
               :library {:east :upstairs-bedroom,
                         :south :back-stairs}})

(defn choices [room]
  "Return a collection of choices which Robbie has when in `room`."
  (rooms room))

(defn look [direction room]
  "Return the room which Robbie can 'see' when looking in `direction` and in
  `room`."
  ((choices room) direction))

(def loc "" :pantry)

(defn set-robbie-location [place]
  "Moves Robbie to `place` by setting the variable `loc`."
  (def loc place))

(defn how-many-choices []
  (count (choices loc)))

(defn upstairs? [room]
  "Return `true` if `room` is upstairs in the house."
  (or (= room :library)
      (= room :upstairs-bedroom)))

(defn on-stairs? [room]
  "Return `true` if `room` is on a set of stairs."
  (or (= room :front-stairs)
      (= room :back-stairs)))

(defn where []
  "Output where Robbie is in the house."
  (let [room-string (name loc)]
    (cond (on-stairs? loc) (str "Robbie is on the " room-string)
          (upstairs? loc)  (str "Robbie is upstairs in the " room-string)
          :else            (str "Robbie is downstairs in the " room-string))))

(defn move [direction]
  "Moves Robbie in `direction`, if possible; outputs descriptive string in both
  cases."
  (let [possible-new-location (look direction loc)]
    (if possible-new-location
      (do (set-robbie-location possible-new-location) (where))
      "Ouch! Robbie hit a wall!")))

(defn royal-we [str]
  "Change every occurrence of 'I' in `str` with 'we'."
  (clojure.string/replace str #" I " " we "))
