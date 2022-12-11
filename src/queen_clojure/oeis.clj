(ns queen-clojure.oeis
  (:require
   [queen-clojure.queen :as queen]))

;; A000201 Lower Wythoff sequence (a Beatty sequence): 
;; a(n) = floor(n*phi), where phi = (1+sqrt(5))/2 = A001622.
(defn lower-wythoff-seq-i [i]
  (let [phi (/ (+ 1 (Math/sqrt 5)) 2)]
    (mapv #(int (Math/floor (* % phi))) (range 1 (inc i)))))

;; A002064Cullen numbers C(n) = n**2^n+1
;; [0 1 2 3 4 5] -> [1 3 9 25 65]
(defn cullen-seq-i [i]
  (mapv #(int (+ 1 (* % (Math/pow 2 %)))) (range 0 i)))

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



;; A088054
(queen/prime? 3)
(queen/primes-under 100)
(mapv inc (mapv queen/factorial (queen/primes-under 8)))
;; (defn factorial-primes-u [n]
;;   (let [primes (primes-under [n])]
;;     ()))



