(ns queen-clojure.queen)

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

(defn factorial [n]
  (loop [n n
         result 1]
    (if (= n 0)
      result
      (recur (dec n) (* n result)))))

(defn factorials-i [n]
  (map factorial (range (+ n 1))))

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




;; [0,1,0,1,0] -> 0
(count [1 2 3])
(group-by odd? [0 1 0 1 0])
(group-by count ["a" "b" "a" "aa" "bb" "b"])

(defn find-odd [xs]
  (let [c (atom 0)
        vec (sort xs)]))

(def atomic-clock (atom 0))

@atomic-clock

"You can change at the swap meet"
(do
  (swap! atomic-clock inc)
  @atomic-clock)

;; 132189  -->  1 + 3 + 2 + 1 + 8 + 9 = 24  -->  2 + 4 = 6
;; 493193  -->  4 + 9 + 3 + 1 + 9 + 3 = 29  -->  2 + 9 = 11  -->  1 + 1 = 2

;; (apply + (map #(Integer/parseInt %) (clojure.string/split (str 132189) #"")))
;; (defn digital-root [n]
;;   (defn func [m]
;;     (apply + (map #(Integer/parseInt %) (clojure.string/split (str m) #""))))
;;   (loop [result 0 f (func m)])
;;   (if (= 1 (count (str n))) result)

;;   ())