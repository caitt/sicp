;; ex 2.17
(defn last-pair [[_ & xs :as list]]
  (if xs (recur xs) list))

;; ex 2.18
(defn reverse [x]
  (loop [x x acc []]
    (if (empty? x)
      acc
      (recur (rest x) (cons (first x) acc)))))


;; ex 2.19
;; Define the procedures first-denomination,
;; except-first-denomination, and no-more? in terms of primitive
;; operations on list structures. Does the order of the list
;; coin-values affect the answer produced by cc? Why or why not?

(def first-denomination first)
(def except-first-denomination rest)
(def no-more? empty?)

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values))))

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(is (= 292 (cc 100 us-coins)))

;; ex 2.20

(comment  ;; non-idiomatic
(defn same-parity
 ( [] '() )
 ( [x & args]
    (cons x (apply same-parity (rest args))))))

(defn same-parity [& args]
  (when-let [s (seq args)]
    (cons (first s) (apply same-parity-2 (nnext s)))))

;; ex 2.21.

(defn- sq [x] (* x x))

(defn square-list [items]
  (when-let [s (seq items)]
    (cons (sq (first s)) (square-list (rest s)))))

(is (= (square-list [ 1 2 3 4 ]) [1 4 9 16]))

(defn square-list [items]
  (map sq items))

(is (= (square-list [ 1 2 3 4 ]) [1 4 9 16]))

;; ex 2.23

(defn for-each [f list]
  (when-let [s (seq list)]
    (do
      (f (first s))
      (recur f (rest s)))))

;; ex 2.24

;; (list 1 (list 2 (list 3 4))) -> (1 2 (3 4))

;; ex 2.25

(let [pick-7 '(1 3 (5 7) 9)
      f (comp first rest first rest rest)]
  (is (= 7 (f pick-7))))


(let [pick-7 '((7))
      f (comp first first)]
  (is (= 7 (f pick-7))))


(let [pick-7 '(1 (2 (3 (4 (5 (6 7))))))
      go-down (comp first next)
      sublists (iterate go-down pick-7)]
  (is (= 7 (nth sublists 6))))


;; ex 2.26
(def x (list 1 2 3))
(def y (list 4 5 6))

(is (= (range 1 7) (concat x y)))
(is (= '((1 2 3) 4 5 6) (cons x y)))
(is (= '((1 2 3) (4 5 6)) (list x y)))

;; ex 2.27
(defn deep-reverse [l]
  (loop [l l acc '()]
    (if (seq l)
      (let [x (first l)
            y (if (list? x) (deep-reverse x) x)]
            (recur (next l) (cons y acc)))
      acc)))

;; ex 2.28
(defn fringe [tree]
  (if (seq? tree)
    (when (seq tree)
      (lazy-cat (fringe (first tree)) (fringe (rest tree))))
    (list tree)))

;; ex 2.29

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(def left-branch first)
(def right-branch second)

(def branch-length first)
(def branch-structure second)


(def test-mobile
  (make-mobile
   (make-branch 10
                5)
   (make-branch 5
                (make-mobile
                 (make-branch 3 2)
                 (make-branch 6 1)))))

(def left-structure (comp branch-structure left-branch))
(def right-structure (comp branch-structure right-branch))
(defn total-weight [mobile]
  (if (list? mobile)
    (+ (total-weight (left-structure mobile))
       (total-weight (right-structure mobile)))
    mobile))

(is (= 8 (total-weight test-mobile)))

(defn is-balanced [mobile]
  (if (list? mobile)
    (let [lb (right-branch mobile)
          rb (left-branch mobile)
          torque #(* (total-weight (branch-structure %))
                     (branch-length %))]
      (and (is-balanced (branch-structure lb))
           (is-balanced (branch-structure rb))
           (=  (torque  rb)
               (torque  lb))))
    mobile))

(def balanced-mobile
  (make-mobile
   (make-branch 5 3)
   (make-branch
    3
    (make-mobile
     (make-branch 3 2)
     (make-branch 2 3)))))

(is (not (is-balanced test-mobile)))
(is (is-balanced balanced-mobile))

;; ex 2.30
(defn square-tree [tree]
  (when-let [s (seq tree)]
    (let [u (first s)]
      (cons (if (list? u)
              (square-tree u)
              (* u u))
            (square-tree (rest s))))))

(defn square-tree-2 [tree]
  (map #(if (list? %)
          (square-tree-2 %)
          (* % %)) tree))

;; ex 2.31

(defn tree-map [f tree]
  (map #(if (list? %)
          (tree-map f %)
          (f %)) tree))

(defn square-tree-3 [tree]
  (tree-map #(* % %) tree))

;; ex 2.32
(defn subsets [set]
  (if-let [[f & r] (seq set)]
    (let [xs (subsets r)]
      (concat xs (map (partial cons f) xs)))
    '(())))

;; ex 2.33
(defn accumulate [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (accumulate op initial (rest sequence)))))


(defn map' [f sequence]
  (accumulate #(cons (f %1) %2) nil sequence))
(defn append' [seq1 seq2]
  (accumulate cons seq2 seq1))
(defn length [sequence]
  (accumulate (fn [_ y] (inc y)) 0 sequence))

(is (= [1 4 9 16] (map' #(* % %) (range 1 5))))
(is (= [1 2 3 4] (append' [1 2] [3 4])))
(is (= 5 (length (range 1 6))))

;; ex 2.34
(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms] (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;; compute 1 + 3x + 5x3 + x5 at x = 2
(is (= 79 (horner-eval 2 (list 1 3 0 5 0 1))))


;; ex 2.35
(comment
  (defn count-leaves [tree]
    (accumulate
     (fn [st acc] (+ acc (if (list? st) (count-leaves st) st)))
     0
     tree)))

(defn count-leaves [tree]
  (accumulate
   (fn [st acc] (+ acc st))
   0
   (map #(if (list? %) (count-leaves %) %) tree)))

;; ex 2.36
(defn accumulate-n [op init seqs]
  (when (seq (first seqs))
      (cons (accumulate op init (map first seqs))
            (accumulate-n op init (map rest seqs)))))

(is (= '(22 26 30)
       (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))))

;; ex 2.37

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map (partial dot-product v) m))

(defn transpose [mat]
  (accumulate-n cons nil mat))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (partial matrix-*-vector cols) m)))

;; ex 2.38
(def fold-right accumulate)
(def fold-left reduce)

(is (= (/ 3 2) (fold-right / 1 (list 1 2 3))))
(is (= (/ 1 6) (fold-left / 1 (list 1 2 3))))

(is (= '(1 ( 2 (3 nil))) (fold-right list nil  (list 1 2 3))))
(is (= '(((nil 1) 2) 3) (fold-left list nil  (list 1 2 3))))

;; for fold-right & fold-left to equal op has to be associative

;; oops this one is quatratic
(defn rev [sequence]
  (fold-right (fn [el col] (concat col (list el)))  nil sequence))

;; linear using CPS; not fitting the requested shape though
(defn rev [sequence]
  ((fold-right (fn [el f] (fn [x] (f (cons el x)))) identity sequence) nil))

(defn rev [sequence]
  (fold-left #(cons %2 %1) nil sequence))

;; ex 2.40
(defn unique-pairs [n]
  (for [i (range 1 (inc n)) j (range 1 i)] [j i]))

(defn- prime? [n]
  (let [divisors (take-while #(<= (* %1 %1) n) (range 2 n))]
    (every?  #(not= 0 %) (map #(rem n %) divisors))))

(comment
(defn prime-sum-pairs [n]
  (map (fn [[x y]] (list x y (+ x y)))
       (filter (fn [[x y]] (prime? (+ x y)))
               (unique-pairs n)))))

(defn prime-sum-pairs [n]
  (->> n
       unique-pairs
       (filter (fn [[x y]] (prime? (+ x y))))
       (map (fn [[x y]] (list x y (+ x y))))))

;; ex 2.41
(defn triples [n s]
  (for [i (range 1 (inc n)) j (range 1 i) k (range 1 j) :when (= (+ i j k) s)]
    [i j k]))

;; ex 2.42
(def safe-position?)
(def adjoin-position)
(def empty-board)

;; redefine above symbols so queens solves n-queens problem
(defn queens [board-size]
  ((fn queen-cols [ k ]   
     (if (= k 0)
       (list empty-board)
       (filter
        #(safe-position? k %)
        (mapcat
         (fn [rest-of-queens]
           (map
            #(adjoin-position % k rest-of-queens)
            (range board-size)))
         (queen-cols (- k 1))))))
   board-size))

;; task : implement functions above
(def empty-board '())

(defn adjoin-position [idx _ position]
  (cons idx position))

(defn safe-position? [_ [t & position]]
  (let [a (repeat t)
        b (iterate inc t)
        c (iterate dec t)
        rays (map rest [a b c])]
    (not-any? boolean (mapcat #(map = % position) rays))))

    
