(ns sicp_clj.chapter1.section2
  )

;; ex 1.11
;; Write a fuction that computes f (recursively and iteratively)
;; f(n) = n for n < 3
;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) for n >= 3

(defn- compute-next-f [a1 a2 a3]
  (+ a1 (* 2 a2) (* 3 a3)))
  
(defn f-recur [n]
  (if (< n 3) n
      (let [a1 (f-recur (- n 1))
            a2 (f-recur (- n 2))
            a3 (f-recur (- n 3))]
        (compute-next-f a1 a2 a3))))

(defn- f-helper [n a1 a2 a3]
      (recur (dec n) a2 a3 (compute-next-f a1 a2 a3)))

(defn f-iter [n]
  (if (< n 3) n
      (f-helper n 1 2 3)))

;; ex 1.12
;; Compute Pascal Triangle
(defn- next-row [row]
      (lazy-cat [1] (map + row (rest row)) [1]))

(defn pascal
  (iterate next-row [1]))

;; ex 1.16
(defn fast-expt [x n]
  (loop [b x n n acc 1]
    (if (= n 0)
      acc
      (recur (sq b)
             (quot n 2)
             (if (odd? n) (* acc b) acc)))))

;; ex 1.17 + 1.18
(defn- halve [a]
  (quot a 2))

(defn- doubl [a]
  (+ a a))

(defn fast-mult [a b]
  (loop [a a b b acc 0]
    (if (zero? a) acc
        (recur (halve a) (doubl b) (+ acc (if (odd? a) b 0))))))

;; ex 1.19
(defn fib-iter [n]
  "Compute n-th fibonnaci number in logarithmic time and constant space."
  ;; We use these identities:
  ;; Fib(n+1),Fib(n) = T_{0,1}^n (1,0)
  ;;     where T_{p,q} (a,b) = (aq+ap+bq, aq+bp)
  ;;     and T^2_{p,q} = T_{p^2+q^2, q^2+2pq}
  ;;
  (loop [a 1 b 0 p 0 q 1 n n]
    (cond (zero? n) b ;; we are done
          (even? n)   ;; we can go from T to T^2 in one step
          (recur a
                 b
                 (+ (sq q) (sq p))
                 (+ (sq q) (* 2 p q))
                 (halve n))
          :else       ;; we have to compute application of T 
          (recur (+ (* a (+ q p)) (* b q))
                 (+ (* a q) (* b p))
                 p
                 q
                 (dec n))
          )))
