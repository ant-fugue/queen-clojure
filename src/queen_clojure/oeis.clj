(ns queen-clojure.oeis
  (:require
   [queen-clojure.queen :as queen]))

;; A000201 Lower Wythoff sequence (a Beatty sequence): 
;; a(n) = floor(n*phi), where phi = (1+sqrt(5))/2 = A001622.
(defn lower-wythoff-seq-i [i]
  (let [phi (/ (+ 1 (Math/sqrt 5)) 2)]
    (mapv #(int (Math/floor (* % phi))) (range 1 (inc i)))))

;; A002110
;; (map )(queen/primes-under 20)
;; (defn primorial-primes-under [n]
;;   (

;;    ))


;; A002064Cullen numbers C(n) = n**2^n+1
;; [0 1 2 3 4 5] -> [1 3 9 25 65]
(defn cullen-seq-i [i]
  (mapv #(int (+ 1 (* % (Math/pow 2 %)))) (range 0 i)))

;; A002310 a(n) = 5*a(n-1) - a(n-2). a(0)=1,a(1)=2

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
  (flatten (map reverse (queen/distinct-int-pairs n))))


