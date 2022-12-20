(ns queen-clojure.queen)

;; utils
(defn binary->dicimal [bin]
  (let [bin-arr (map #(Integer/parseInt %) (clojure.string/split  bin #""))
        reversed-indices (reverse (range 0 (count bin-arr)))
        func #(int (* %1 (Math/pow 2 %2)))]
    (apply + (map #(func %1 %2) bin-arr reversed-indices))))

(defn digit-sum [n]
  (apply + (map #(Integer/parseInt %) (clojure.string/split (str n) #""))))

(defn distinct-int-pairs [n]
  (apply concat
         (map (fn [i] (map (fn [j] (list i j)) (range 1 i)))
              (range 1 (inc n)))))

(defn div-of [n]
  (loop [i n
         lst '()]
    (cond (zero? i) lst
          (zero? (mod n i)) (recur (dec i) (conj lst i))
          :else (recur (dec i) lst))))

(defn num-of-div [n]
  (count (div-of n)))

(defn div-sum [n]
  (apply + (div-of n)))

(defn div-of-except-self [n]
  (let [coll (vec (div-of n))]
    (cond (zero? (count coll)) '()
          (= 1 (count coll)) '()
          :else (subvec coll 0 (dec (count coll))))))

(defn num-of-div-except-self [n]
  (count (div-of-except-self n)))

(defn div-sum-except-self [n]
  (apply + (div-of-except-self n)))

(defn deficient? [n]
  (cond  (zero? n) false
         (> n (div-sum-except-self n)) true
         :else false))

(defn abundant? [n]
  (cond  (zero? n) false
         (< n (div-sum-except-self n)) true
         :else false))

(defn perfect? [n]
  (cond  (zero? n) false
         (= n (div-sum-except-self n)) true
         :else false))

;; [1 2 3 4 6 12] -> [2 4 6]
;; (defn semi-perfect? [n]
;;   )

(defn factorial [n]
  (loop [n n
         result 1]
    (if (= n 0)
      result
      (recur (dec n) (* n result)))))

(defn factorials-i [n]
  (mapv factorial (range (+ n 1))))


;; (prime? 6) -> [2 3 4 5] -> false
(defn prime? [n]
  (defn search [n]
    (loop [num-list (range 2 n)
           i 2]
      (cond  (empty? num-list) true
             (zero? (mod n i)) false
             :else (recur (rest num-list) (inc i)))))
  (if (= n 1)
    false
    (search n)))

;; revise later to add range definition
(defn primes-under [n]
  (filter prime? (range 2 (+ n 1))))

(defn prime-factors [n]
  (loop [n n
         result []
         div 2]
    (cond (< n 2) result
          (zero? (mod n div)) (recur (/ n div) (conj result div) div)
          :else (recur n result (inc div)))))


;; A005117 isSquarefree numbers
(defn square-free? [num]
  (let [prime-factors (prime-factors num)
        prime-factors-length (count prime-factors)
        prime-factors-set-length (count (set prime-factors))]
    (cond (= num 0) false
          (= num 1) true
          (= prime-factors-length prime-factors-set-length) true
          :else false)))

(defn square-free-numbers-under [n]
  (filterv square-free? (range 0 (inc n))))

(defn derange [n]
  (cond (zero? n) 1
        (= 1 n) 0
        :else (* (dec n) (+ (derange (dec n)) (derange (- n 2))))))

(derange 5)

(defn derangements-under [i]
  (map derange (range 0 (inc i))))




;; [0,1,0,1,0] -> 0
(count [1 2 3])
(group-by odd? [0 1 0 1 0])
(group-by count ["a" "b" "a" "aa" "bb" "b"])

;; (defn find-odd [xs]
;;   (let [c (atom 0)
;;         vec (sort xs)]))
