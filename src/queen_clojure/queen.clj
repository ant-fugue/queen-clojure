(ns queen-clojure.queen)

;; A002064Cullen numbers C(n) = n**2^n+1
;; [0 1 2 3 4 5] -> [1 3 9 25 65]
(defn cullen-seq [n]
  (map #(int (+ 1 (* % (Math/pow 2 %)))) (range 0 n)))


(cullen-seq 5)

;; (defn div-of [n]
;;   (loop [i n
;;          lst '()]
;;     (cond (zero? i) lst
;;           (zero? (mod n i)) (recur (inc i) (conj lst i))
;;           :else (recur (inc i) lst))))

;; (div-of 6)

(->>
 [1 2 3 4 5]
 (#(map inc %)))

(defn factorial [n]
  (loop [n n
         result 1]
    (if (= n 0)
      result
      (recur (dec n) (* n result)))))

(factorial 5)

  ;; (let [x 5]
  ;;   (= :your-road (cond (= x 3) :road-not-taken
  ;;                       (= x 4) :another-road-not-taken
  ;;                       :else :your-road)))

;; (defn prime? [n]
;;   (loop [vec (range 2 n)
;;          i n]
;;     (if (zero? (% n i))
;;       false
;;       (recur (rest vec) ))))

(range 2 10)
(if (zero? (mod 10 2)) false true)
(if (zero? (mod 10 3)) false true)

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
    (= n (search n))))



(prime? 3)
(prime? 6)
(prime? 1)
(when (= 1 1) false)

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