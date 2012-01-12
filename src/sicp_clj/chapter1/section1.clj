(ns sicp_clj.chapter1.section1
  )

;; exc. 1.2
(/ (+ 5 4 (- 2 (- (- 3 (+ 6 (/ 4 5))))))
   (* 3 (- 6 2) (- 2 7) ))

;; exc 1.3
(defn- sq [x] (* x x))
(defn- sum-sq [x y] (+ (sq x) (sq y)))

(defn ex-1-3 [x y z]
  (cond
   (and (< x y) (< x z)) (sum-sq y z)
   (and (< y x) (< y z)) (sum-sq x z)
   :true                 (sum-sq x y)))

;; ex 1.8
(defn- improve-cube [x y]
  "Improving cube root guess via Newton method"
  (/ (+ (/ x (sq y)) (* 2 y)) 3))

(defn- good-enough-cube? [x g]
  (< (sq (- x (* g g g))) 0.000001))
    
(defn- cube-newton [x guess]
  (if (good-enough-cube? x guess)
    guess
    (cube-newton x (improve-cube x guess))
    ))

(defn cube-root [n]
  (cube-newton n 1.0))

