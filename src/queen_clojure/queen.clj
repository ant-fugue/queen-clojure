(ns queen-clojure.queen
  (:require [clojure.string :as str]))

;; utils
(defn positions
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn binary->dicimal [bin]
  (let [bin-arr (map #(Integer/parseInt %) (str/split  bin #""))
        reversed-indices (reverse (range 0 (count bin-arr)))
        func #(int (* %1 (Math/pow 2 %2)))]
    (apply + (map #(func %1 %2) bin-arr reversed-indices))))

(defn digit-sum [n]
  (->> (str/split (str n) #"")
       (map #(Integer/parseInt %))
       (apply +)))

(defn distinct-int-pairs [n]
  (for [x (range 2 (inc n))
        y (range 1 n) :while (> x y)]
    [x y]))

;; (defn pytagorean-int-trios [n]
;;   (let [s1 (distinct-int-pairs n)
;;         sq-and-sum (fn [a b] (+ (* a a) (* b b)))
;;         f ()
;;         s2 (map #(reduce sq-and-sum %) (distinct-int-pairs n))]
;;     (->>
;;      (map #(conj %1 %2) s1 s2)
;;      (filter (fn [n] (let [root (Math/sqrt (last n))]
;;                        (== root (int root)))))
;;      (map #(int (Math/sqrt (last %)))))))

;; (pytagorean-int-trios 20)

;; cat div operations
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

(defn div-group [n]
  (cond (= n (div-sum-except-self n)) "perfect"
        (< n (div-sum-except-self n)) "abundant"
        (> n (div-sum-except-self n)) "deficient"))

(defn coprime? [m n]
  (let [m-div (div-of m)
        n-div (div-of n)]
    (->> (concat m-div n-div)
         (remove #{1})
         (frequencies)
         (vals)
         (every? #(= % 1)))))

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
  (let [search (fn [n]
                 (loop [num-list (range 2 n) i 2]
                   (cond  (empty? num-list) true
                          (zero? (mod n i)) false
                          :else (recur (rest num-list) (inc i)))))]
    (if (= n 1)
      false
      (search n))))



;; revise later to add range definition
(defn prime-seq
  ([max]
   (filter prime? (range 2 (inc max))))
  ([min max]
   (filter prime? (range min (inc max)))))

;; [2 3 5 7]
;; current is 5 -> generate [2 3 5] -> (inc (apply * [2 3 5]))
(defn euclid-seq-under [n]
  (->> (prime-seq n)))

(defn composite? [n]
  (if (= n 1)
    false
    (not (prime? n))))

(defn composites-under [n]
  (filterv composite? (range 2 n)))

(defn highly-composite? [n]
  (->> (range 1 n)
       (mapv num-of-div)
       (every? #(< % (num-of-div n)))))

;; A002182
(defn highly-composite-under [n]
  (filterv highly-composite? (range 1 n)))

(defn prime-factors [n]
  (loop [n n
         result []
         div 2]
    (cond (< n 2) result
          (zero? (mod n div)) (recur (/ n div) (conj result div) div)
          :else (recur n result (inc div)))))

(defn semiprime? [n]
  (let [pf (prime-factors n)]
    (if (= (count pf) 2)
      true
      false)))

(defn semi-prime-under [n]
  (filterv semiprime? (range 1 n)))

;; !間違い
(defn perfect-power? [n]
  (->> (prime-factors n)
       (set)
      ;;  (= 1 count)
       ))

(perfect-power? 36)

(defn perfect-power-under [n]
  (filterv perfect-power? (range 1 n)))

(defn is-perfect-square? [n]
  (let [sqrt-n (int (Math/sqrt n))]
    (= (* sqrt-n sqrt-n) n)))


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
  (filterv square-free? (range 0 n)))

(defn derange [n]
  (cond (zero? n) 1
        (= 1 n) 0
        :else (* (dec n) (+ (derange (dec n)) (derange (- n 2))))))

(derange 5)

(defn derangements-under [i]
  (map derange (range 0 (inc i))))

(defn prime-gaps [n]
  (map - (prime-seq 3 n) (prime-seq n)))




;; (defn find-odd [xs]
;;   (let [c (atom 0)
;;         vec (sort xs)]))

;; (dotest 1071625, 1103735, "(1081184 1331967)")
;; (dotest 57345, 90061, "(62744 75495)")
;; (dotest 2382, 3679, "Nothing")
;; (dotest 8983, 13355, "(9504 20735)")

;; Divisors of 48: 1, 2, 3, 4, 6, 8, 12, 16, 24 --> sum: 76 = 75 + 1
;; Divisors of 75: 1, 3, 5, 15, 25 --> sum: 49 = 48 + 1

;; (n, m) are a pair of buddy if s (m) = n + 1 and s (n) = m + 1

(defn buddy [start nd]
  (let [num-seq (vec (range start (inc nd)))
        sorted-pair-seq (->> num-seq
                             (mapv #(concat [] [% (dec (div-sum-except-self %))]))
                             (mapv sort))
        buddy (->> sorted-pair-seq
                   (map  #(when (<= 2 (count (positions #{%} sorted-pair-seq))) %))
                   (remove nil?)
                   (distinct)
                   (first))]
    buddy))

(buddy 45 80)



;; (def v [[1 2] [2 3] [1 2]])
;; (map #(if (<= 2 (count (positions #{%} v)))
;;         (conj result)) v)





;; (map #(.indexOf "this" %) ["this" "pen" "this"])
;; (map #(.lastIndexOf "this" %) ["this" "pen" "this"])
;; (clojure.set/difference (set [[48 75] [49 7]]) (set [[48 75] [49 7] [75 48]]))

;; (deftest example-tests
;;   (is (= (array-diff [1 2] [1]) [2]))
;;   (is (= (array-diff [1 2 2] [1]) [2 2]))
;;   (is (= (array-diff [1 2 2] [2]) [1]))
;;   (is (= (array-diff [1 2 2] []) [1 2 2]))
;;   (is (= (array-diff [1 2 3] [1 2]) [3]))
;;   (is (= (array-diff [] [1 2]) [])))

(not (contains? #{[3]} [1 2]))
#{[1 2]}


;; (max-sequence [-2, 1, -3, 4, -1, 2, 1, -5, 4])
;; should be 6: [4, -1, 2, 1]

(defn max-sequence [xs])


(hash-map :all [1 2 3 4 5])

;; (defn f [seq]
;;   (let [s1
;;         (->> seq
;;              (mapv #(str/split % #" "))
;;              (mapv #(vec (conj [(str (nth (first %) 0))] (read-string (last %)))))
;;              (map #(hash-map (keyword (str (first %))) (last %)))
;;              (hash-map :all)
;;              (merge-with +))
;;         s2 []]
;;     s1))



;; (f ["BBAR 150", "CDXE 515", "BKWR 250", "BTSQ 890", "DRTY 600"])


;; (apply #(merge-with + %1 %2 %3) '({:B 150} {:C 200} {:B 300}))
(map #(contains? % :B) '({:B 150} {:C 200} {:B 300}))

;; (into {}
;;       (map
;;        (fn [[k v]] [k (inc v)])
;;        {:a 1 :b 2 :a 3}))

;; (defn div-of-except-self [n]
;;   (loop [i (int (/ n 2))
;;          lst '()]
;;     (cond (zero? i) lst
;;           (zero? (mod n i)) (recur (dec i) (conj lst i))
;;           :else (recur (dec i) lst))))

;; (defn div-sum-except-self [n]
;;   (apply + (div-of-except-self n)))

;; (div-sum-except-self 10)

;; (deftest Tests
;;   (is (= (human-readable      0) "00:00:00"))
;;   (is (= (human-readable     59) "00:00:59"))
;;   (is (= (human-readable     60) "00:01:00"))
;;   (is (= (human-readable     90) "00:01:30"))
;;   (is (= (human-readable  86399) "23:59:59"))
;;   (is (= (human-readable 359999) "99:59:29")))

;; (def a 359949)

;; (defn f [n]
;;   (let [q (atom 0)
;;         r (atom 0)]
;;     (swap! q #(quot n 3600))
;;     (swap! r #(rem n 60))
;;     [@q @r]))

;; (f 359949)

;; cat fibonacci variants
;; basic, but very slow recursive function
;; (defn fib [n]
;;   (if (or (zero? n) (= n 1))
;;     n
;;     (+ (fib (dec n)) (fib (- n 2)))))

(defn fib [n]
  (let [root5 (Math/sqrt 5)]
    (->>
     (* (/ 1 root5)
        (- (Math/pow (/ (+ 1 root5) 2) n)
           (Math/pow (/ (- 1 root5) 2) n)))
     (int))))

(defn fib-seq-under [n]
  (let [fib-seq (->> (iterate (fn [[a b]] [b (+ a b)]) [0 1])
                     (take-while #(<= (first %) n)))]
    (mapv first fib-seq)))

;;; error check later
;; (defn is-fib? [n]
;;   (let [lst (fib-seq-under n)]
;;     (some #{n} lst)))

(defn zeckendorf-exp [n]
  (if (or (nil? n) (not (number? n)) (< n 0))
    (throw (IllegalArgumentException. "Input must be a non-negative integer."))
    (let [fibs (fib-seq-under n)]
      (loop
       [x n
        fibs fibs
        result []]
        (cond
          (= n (apply + result)) (vec (reverse result))
          (empty? fibs) (vec (reverse result))
          :else
          (let [largest-fib (last fibs)
                remaining (- x largest-fib)]
            (if (>= remaining 0)
              (recur remaining (butlast fibs) (conj result largest-fib))
              (recur x (butlast fibs) result))))))))

;; (defn lucas [n]
;;   (cond (zero? n) 2
;;         (= n 1) 1
;;         :else (+ (lucas (dec n)) (lucas (- n 2)))))

(defn lucas [n]
  (let [root5 (Math/sqrt 5)]
    (->>
     (+ (Math/pow (/ (+ 1 root5) 2) n)
        (Math/pow (/ (- 1 root5) 2) n))
     (int))))

(defn tribonacci [n]
  (cond (zero? n) 0
        (= n 1) 0
        (= n 2) 1
        :else (+ (tribonacci (dec n))
                 (tribonacci (- n 2))
                 (tribonacci (- n 3)))))

;; cat unclassified
(defn kaprekar-routine [n]
  (let [min (->>
             (str n)
             (sort)
             (str/join)
             (Integer/parseInt))
        max (->>
             (str n)
             (sort #(compare %2 %1))
             (str/join)
             (Integer/parseInt))]
    (- max min)))

(defn kaprekar-routine-seq [n]
  (loop [result []
         current n]
    (let [next (kaprekar-routine current)]
      (if (or (= next 495) (= next 0))
        (conj result next)
        (recur (conj result next) next)))))

(defn primes-seq-i [n]
  (let [numbers (vec (range 2 (+ n 1)))
        primes (vec (filter (fn [x] (nth numbers x)) numbers))]
    (doseq [prime primes]
      (loop [p (* prime prime) n (inc prime)]
        (when (<= p n)
          (when (nth numbers n)
            (aset numbers n nil))
          (recur (+ p prime) (inc n)))))
    (take n primes)))

;; (primes-seq-i 5)

(defn primes-until [index]
  (let [numbers (vec (range 2 (+ index 1)))
        primes (vec (filter (fn [x] (nth numbers x)) numbers))]
    (doseq [prime primes]
      (loop [p (* prime prime) n (inc prime)]
        (when (<= p index)
          (when (nth numbers n)
            (aset numbers n nil))
          (recur (+ p prime) (inc n))))
      (println prime))
    primes))


;; (mapv vector (range 10 30) (mapv zeckendorf-exp (range 10 30)))
(div-sum-except-self 128)
(div-sum-except-self 6)
(prime-factors 136)
(deficient? 63)
(deficient? 18)
(deficient? 99)
