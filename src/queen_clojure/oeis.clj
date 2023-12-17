(ns queen-clojure.oeis
  (:require
   [queen-clojure.queen :as queen]))

;; A000201 Lower Wythoff sequence (a Beatty sequence): 
;; a(n) = floor(n*phi), where phi = (1+sqrt(5))/2 = A001622.
(defn lower-wythoff-seq-i [i]
  (let [phi (/ (+ 1 (Math/sqrt 5)) 2)]
    (->> (range 1 (inc i))
         (mapv #(int (Math/floor (* % phi)))))))

;; A000396(perfect number sequence)
(defn perfect-seq [n]
  (filterv queen/perfect? (range 0 n)))

;; A001694
(defn powerful? [n]
  (->> (queen/prime-factors n)
       (set)
       (vec)
       (mapv #(zero? (mod n (* % %))))
       (every? true?)))

(defn powerful-seq-under [n]
  (filterv powerful? (range 1 n)))

;; A052486　Achilles numbers
(defn achilles? [n]
  (cond (not (powerful? n)) false
        (and (powerful? n) (not (queen/perfect? n))) true
        :else false))

(defn achilles-seq-under [n]
  (filterv achilles? (range 1 n)))


;; A002110
;; (map )(queen/primes-under 20)
;; (defn primorial-primes-under [n]
;;   (

;;    ))


;; A002064Cullen numbers C(n) = n**2^n+1
;; [0 1 2 3 4 5] -> [1 3 9 25 65]
(defn cullen-seq-i [i]
  (->> (range 0 i)
       (mapv #(int (+ 1 (* % (Math/pow 2 %)))))))

;; A003387
;; (defn A003387-seq-i [i]
;;   (mapv #()))

;; A005165 Altenating factorial n! - (n-1)! + (n-2)! - ... 1!

;; [1 2 3 4 5 6] -> [1 1 5 19 101]
(defn alt-fact [n]
  (loop [n n
         result (queen/factorial n)
         positive? true]
    (cond (zero? n) nil
          (= n 1) result
          (= positive? true) (recur (dec n) (- result (queen/factorial (dec n))) (not positive?))
          :else (recur (dec n) (+ result (queen/factorial (dec n))) (not positive?)))))

;; A005384 Sophie Germain primes p: 2p+1 is also prime.
(defn sophie-germain-p? [n]
  (if (and (queen/prime? n) (queen/prime? (inc (* 2 n))))
    true
    false))

(defn sophie-germain-p-under [n]
  (filter sophie-germain-p? (queen/prime-seq n)))
;; A01088 digital root
;; 132189  -->  1 + 3 + 2 + 1 + 8 + 9 = 24  -->  2 + 4 = 6
(defn digital-root [n]
  (defn f [num]
    (apply + (map #(Integer/parseInt %) (clojure.string/split (str num) #""))))
  (loop [tmp n]
    (if (and (<= 0 tmp) (< tmp 10))
      tmp
      (recur
       (f tmp)))))


;; A023106
;; 	a (n) is a power of the sum of its digits.
(defn power-sum-dig? [n]
  (loop [digit-sum (queen/digit-sum n)
         i 0]
    (cond
      (< n 10) true
      ;; avoid infinite loop when given number is the combination of first 1 and others 0.
      (<= digit-sum 1) false
      (< n (int (Math/pow digit-sum i))) false
      (= n (int (Math/pow digit-sum i))) true
      :else (recur digit-sum (inc i)))))

(defn power-sum-seq-under [n]
  (filter power-sum-dig? (range 0 n)))

;; A088054
;; [ 2 3 5   7    11       ]
;; [ 3 7 121 5041 39916801 ]
;; (defn factorial-primes-u [n]
;;   (let [primes (queen/primes-under [n])
;;         len (count primes)
;;         tmp (mapv inc (mapv queen/factorial primes))]
;;     (loop [i 0
;;            result []]
;;       (cond (> i len) result
;;             (queen/prime? (nth tmp i)) (recur (inc i) (conj result (nth tmp i)))
;;             :else (recur (inc i) result)))))


;; (factorial-primes-u 6)

(defn A097285-seq-under [n]
  (->> (queen/distinct-int-pairs n)
       (map reverse)
       (flatten)))

(defn sq [n] (* n n))

(mod 4 2)

;; A000035
;; (map #(mod (int (Math/pow % 2)) 2) (range 0 10))


;; (defn digit-sum [n]
;;   (apply + (map #(Integer/parseInt %) (clojure.string/split (str n) #""))))

;; (defn custom-log [num base]
;;   (/ (Math/log num) (Math/log base)))

;; (defn power-sum-dig? [n]
;;   (let [dsum (digit-sum n)]
;;     (if (or (< n 10) (<= dsum 1))
;;       false
;;       (->>
;;        (custom-log n dsum)
;;        (Math/pow dsum)
;;        (int)
;;       ;;  (= n)
;;        ))))

;; (power-sum-dig? 35)

;; fibonacci variants
;; A002310 a(n) = 5*a(n-1) - a(n-2). a(0)=1,a(1)=2
(defn A002310 [i]
  (cond (zero? i) 1
        (= 1 i) 2
        :else (- (* 5 (A002310 (dec i))) (A002310 (- i 2)))))

(defn A002310-seq-i [i]
  (->> (range 0 i)
       (mapv A002310)))

(defn perrin [i]
  (cond (zero? i) 3
        (= 1 i) 0
        (= 2 i) 2
        :else (+ (perrin (- i 2)) (perrin (- i 3)))))

(defn perrin-seq-i [i]
  (->> (range 0 i)
       (mapv perrin)))

(defn A003815 [i]
  (if (zero? i)
    0
    (+ i (* (int (Math/pow -1 i)) (A003815 (dec i))))))

(defn A003815-seq-i [i]
  (->> (range 0 i)
       (mapv A003815)))

(defn hofstadter-g [i]
  (if (zero? i)
    0
    (- i (hofstadter-g (hofstadter-g (dec i))))))

(defn hofstadter-g-seq-i [i]
  (->> (range 0 i)
       (mapv hofstadter-g)))



;; consecutive natural numbers are coprime, so this sequence is the same with the nat sequence
;; (defn successive-coprimes-under [n]
;;   (loop [i 1
;;          v []]
;;     (cond (= i n) v
;;           (queen/coprime? i (inc i)) (recur  (inc i) (conj v i))
;;           :else (recur (inc i) v))))

;; (successive-coprimes-under 1000)

;; A005349
(defn harshad? [n]
  (zero? (mod n (queen/digit-sum n))))

(defn harshad-seq-under [n]
  (filterv harshad? (range 1 n)))

;; (defn motzkin-gen [n]
;;   (/ (* 2 (queen/factorial n)) ))

;; Mn = (2n)! / ((n + 1)! * n!)
(defn motzkin-gen [n]
  (->> (queen/factorial (* 2 n))
       (/ (* (queen/factorial (inc n))
             (queen/factorial n)))))

(defn motzkin-seq-under-i [i]
  (->> (range 0 i)
       (mapv motzkin-gen)))

(motzkin-seq-under-i 5)

(filter queen/square-free? [46 30])

(filter harshad? [137 175 135 131])
(filter harshad? [111 112 121 118])
(filter queen/square-free? [138 168 136 134])
(filter queen/abundant? [138 134])
(queen/prime-factors 112)