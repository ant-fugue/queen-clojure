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
(defn prime-seq
  ([max]
   (filter prime? (range 2 (inc max))))
  ([min max]
   (filter prime? (range min (inc max)))))

(defn composite? [n]
  (if (= n 1)
    false
    (not (prime? n))))

(defn composites-under [n]
  (filter composite? (range 2 (inc n))))

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

;; fibonacci variants
(defn fib [n]
  (if (or (zero? n) (= n 1))
    n
    (+ (fib (dec n)) (fib (- n 2)))))

(defn lucas [n]
  (cond (zero? n) 2
        (= n 1) 1
        :else (+ (lucas (dec n)) (lucas (- n 2)))))

(defn tribonacci [n]
  (cond (zero? n) 0
        (= n 1) 0
        (= n 2) 1
        :else (+ (tribonacci (dec n))
                 (tribonacci (- n 2))
                 (tribonacci (- n 3)))))