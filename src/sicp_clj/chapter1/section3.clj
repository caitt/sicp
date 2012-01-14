(ns sicp_clj.chapter1.section3
  (:use clojure.test))

;; "sum" from book
(defn sum [term a next b]
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; slowly converges to π/8
(defn pi-sum [a b] (sum #(/ 1.0 (* % (+ % 2))) a #(+ % 4) b))

;; relatively close to π
(* 8 (pi-sum 1 1000))  

;; ex 1.29
(defn simpson-rule [f a b n]
  (let [ h (/ (- b a) n) ; use floating numbers!!
        term (fn [i]
               (let [ coef 
                     (cond (= 0 i) 1
                           (= n i) 1
                           (odd? i) 4
                           :else   2)
                     point (+ a (* i h))]
                 (* coef (f point))))]
    (* (/ h 3) (sum term 0 inc n))))


;; ex 1.30
;; sum using iterative process
(defn sum-iter [term a next b]
  (loop [a a result 0]
    (if (> a b)
      result
      (recur (next a) (+ result (term a))))))

;; sum using high-order functions
(defn sum-idiomatic [term a next b]
  (reduce #'+ 0
          (map term (take-while #(< % b) (iterate next a)))))





;;
(defn- abs [x]
  (Math/abs x))

(def eps 0.00001)

(defn close-enough? [x y]
  (< (abs (- x y)) eps))

(defn fix-point [f start]
  (loop [[x1 & stream]  (iterate f start)]
    (let [[x2] stream]
      (if (close-enough? x1 x2)
        x2
        (recur stream)))))


;; ex 1.35
(defn golden-ratio []
  "ø using fixpoint"
  (fix-point #(inc (/ %)) 1.0))

(defn average [x y]
  (/ (+ x y) 2))

(defn average-damp [f]
  #(average % (f %)))

(defn sqrt-fp [x]
  "Sqrt defined using fixpoint"
  ;;  (fix-point #(average % (/ x %)) 1.0))
  (fix-point (average-damp #(/ x %)) 1.0))


;; ex 1.36
(defn instrumented-fixpoint [f start]
  "Same as fixpoint but prints statistics"
  (loop [[x1 & stream]  (iterate f start)]
    (let [[x2] stream]
      (println x2)
      (if (close-enough? x1 x2)
        x2
        (recur stream)))))

(defn- xx-eq [x]
  "Fuction for equation x^x = 1000"
  (/ (Math/log 1000) (Math/log x)))

;; takes 33 steps
(instrumented-fixpoint xx-eq 2.0)

;; takes 8 steps
(instrumented-fixpoint (average-damp xx-eq) 2.0)



;; ex 1.37

(defn cont-frac [n d k]
  "k-term finite continued fraction"
  (loop [k k acc 1]
    (/ (n k) (d k))))

(defn cont-frac-recursive [n d k]
  "k-term finite continued fraction"
  (letfn [(f [i]
          (if (= i k)
            (/ (n k) (d k))
            (/ (n i) (+ (d i) (f (inc i))))))]
    (f 1)))

(defn cont-frac-iter [n d k]
  "k-term finite continued fraction"
  (loop [i k acc 0]
    (if (= 0 i)
      acc
      (recur (dec i) (/ (n i) (+ (d i) acc))))))

;; 10 step is enough to get first 4 digits correct
(/ (cont-frac-iter (constantly 1.0) (constantly 1.0) 10))


;; ex 1.38

(defn- e-2-seq [i]
  (let [q (quot i 3)
        r (rem i 3)]
    (if (= r 2)
      (* (inc q) 2)
      1)))



;; approximation of euler number
(+ 2
   (cont-frac-iter (constantly 1.0) e-2-seq 1000))


;; ex 1.39
(defn tan-cf [x k]
  (letfn [(n [i] (if (= i 1) x (- (sq x))))
          (d [i] (dec (* 2 i)))]
    (cont-frac-iter n d k)))

;; some more cont-frac stuff

(defn square-root-of-2 [k]
  (inc (cont-frac-iter (constantly 1.0) (constantly 2.0) k)))


;; ex 1.41

(defn twice [f]
  #(f (f %)))

(is (= 21 (((twice (twice twice)) inc) 5) ))

;; ex 1.42
(defn compose [f g]
  #(f (g %)))

(is (= 49 ((compose sq inc) 6)))

;; ex 1.43
(defn repeated [f n]
  #(nth (iterate f %) n))

(is (= 625
       ((repeated sq 2) 5)))

