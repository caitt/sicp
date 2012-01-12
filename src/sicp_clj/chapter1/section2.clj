(ns sicp_clj.chapter1.section2
  )

;; ex 11.1
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

;; exc.: 1.12
;; Compute Pascal Triangle
(defn- next-row [row]
      (lazy-cat [1] (map + row (rest row)) [1]))

(defn pascal
  (iterate next-row [1]))