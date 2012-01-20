(ns sicp_clj.chapter2.section1
  :use 'clojure.test)

;; ratio is abstraction defined via functions make-rat, numer and denom
(def make-rat)
(def numer)
(def denom)

;; operation based on this abstraction
(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [x]
  (println (numer x) "/" (denom x)))

;; simple implementation slightly different from the book (clojure
;; does not have primitive pair datatype
(defn make-rat [x y] [x y])
(def denom second)
(def numer first)



;; ex 2.1

;; some helper functions
(defn- abs [a] (Math/abs a))
;; not-exactly correct (gcd 0 0) should be undefined
(defn- gcd [a b]
  (if (zero? b)
    (abs a)
    (recur b (rem a b))))

;; now we ensure that the ratio is in canonical form
;; no change to other methods except make-rat is necessary

(defn make-rat [a b]
  (assert (not= 0 b) "Denominator can't be zero")
  (let [d (gcd a b)
        f (if (neg? b) -1 1)]
    (map (partial * f) [ (/ a d) (/ b d)])))

(let [x (make-rat  2  4)
      y (make-rat -2  4)
      z (make-rat  2 -4)
      w (make-rat -2 -4)]
  (is (equal-rat? x w))
  (is (equal-rat? y z)))


;; ex 2.2

(defn make-seg [start end] [start end])
(defn start-seg [[x _]] x)
(defn end-seg [[_ y]] y)

(defn make-point [x y] [x y])
(defn x-point [[x _]] x)
(defn y-point [[_ y]] y)

;; no destructuring as we are preserving the abstraction
(defn print-point [x]
  (println "(" (x-point x) "," (y-point x) ")"))

(defn mid-point [s]
  (let [a (start-seg s)
        b (end-seg s)]
    (make-point (/ (+ (x-point a) (x-point b)) 2)
                (/ (+ (y-point a) (y-point b)) 2))))

(let [m (mid-point (make-seg (make-point 0 0) (make-point 8 10)))]
  (is (= 4 (x-point m)))
  (is (= 5 (y-point m))))

;; ex 2.3

;; direct representation
(defn make-rect
  "Make rectangle from bottom-left and top-right corner.
   No checks on arguments."
  [a b]
  (assert (<= (x-point a) (x-point b)))
  (assert (<= (y-point a) (y-point b)))
  [a b])
  

(defn top-left [[a b]]
  (make-point (x-point a) (y-point b)))

(defn top-right [[a b]]
  b)

(defn bot-left [[a b]]
  a)

(defn bot-right [[a b]]
  (make-point (x-point b) (y-point a)))

(def test-rect (make-rect (make-point 0 0) (make-point 8 10)))

(let [r (make-rect (make-point 0 0) (make-point 8 10))
      tl (top-left r)
      br (bot-right r)]
  (is (= 8 (x-point br)))
  (is (= 0 (y-point br)))
  (is (= 0 (x-point tl)))
  (is (= 10 (y-point tl))))

;; function based on rect abstraction
(defn dim-rect [r]
  [ (- (x-point (bot-right r)) (x-point (bot-left r)))
    (- (y-point (top-right r)) (y-point (bot-right r)))])

(defn perimeter-rect [r]
  (->> (dim-rect r)
      (apply +)
      (* 2)))
;    (* 2 (apply + (dim-rect r))))

(defn area-rect [r]
  (apply * (dim-rect r)))

;; alternative representation
(defn make-rect [a b]
  (let [ w (- (x-point b) (x-point a))
        h (-  (y-point b) (y-point a))]
    (assert (pos? w))
    (assert (pos? h))
    {:width w
     :height h
     :origin a}))

(defn top-left [{w :width, h :height, a :origin}]
  (make-point (x-point a) (+ (y-point a) h)))

(defn top-right [{w :width, h :height, a :origin}]
  (make-point (+ (x-point a) w) (+ (y-point a) h)))

(defn bot-left [{w :width, h :height, a :origin}]
  a)

(defn bot-right [{w :width, h :height, a :origin}]
  (make-point (+ (x-point a) w)  (y-point a)))

;; we could make area and perimeter more efficient with this
;; representation but that would broken the abstraction
;; dim-rect would move from abstract layer (area, perimeter)
;; to concrete layer (make-rect, top-left, bot-right ...)
(defn dim-rect [{w :width, h :height}] [ w h ])

;; ex. 2.4 - on how to define pair in pure lambda-calculus

(defn cons [x y]
  #(% x y))

(defn car [z]
  (z (fn [p _] p)))

;; task: define cdr
(defn cdr [z]
  (z (fn [_ q] q)))

(defn test-cons-impl []
  (let [c (cons 10 2)]
    (is (= 10 (car c)))
    (is (= 02 (cdr c)))))

;; ex 2.5 redefine cons using arithmetic encoding

;; no standarnd pow in clojure
;; use java bigint as the numbers will be large
(defn- pow [base n]
  (.pow (biginteger base) n))

(defn cons [x y]
  (* (pow 2 x) (pow 3 y)))

(defn- which-power [base x]
  (loop [x x i 0]
    (if (zero? (rem x base))
      (recur (/ x base) (inc i))
      i)))

(def car (partial which-power 2))
(def cdr (partial which-power 3))

;; ex 2.6 - Church encoding

;; define church numerals for one and two
(def church-one (fn [f] (fn [x] (f x))))
(def church-two (fn [f] (fn [x] (f (f x)))))

(defn church-add [a b]
  (fn [f] (fn [x] ((b f) ((a f) x)))))

(let [sum (church-add church-one church-two)]
  (is (= 1 ((church-one inc) 0)))
  (is (= 3 ((sum inc) 0))))
