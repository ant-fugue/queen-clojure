(ns queen-clojure.numbers
  (:require [clojure.string :as str]
            [queen-clojure.queen :as queen]
            [queen-clojure.oeis :as oeis]))

(defn reveal-characteristics [n]
  (let [message (atom [])]
    (if (queen/prime? n)
      (do
        (swap! message conj "prime")
        (if (oeis/sophie-germain-p? n) (swap! message conj "sophie germain prime") (swap! message conj "!sophie germain prime")))
      (do
        (swap! message conj "!prime")
        (if (queen/semiprime? n) (swap! message conj "semiprime") (swap! message conj "!semiprime"))
        (if (queen/highly-composite? n) (swap! message conj "highly-composite") (swap! message conj "!highly-composite"))
        (if (queen/square-free? n) (swap! message conj "square free") (swap! message conj "!square free"))
        (if (oeis/powerful? n) (swap! message conj "powerful") (swap! message conj "!powerful"))))
    (swap! message conj (queen/div-group n))
    (if (oeis/power-sum-dig? n) (swap! message conj "power of the sum of its digits") (swap! message conj "!power of the sum of its digits"))
    (if (oeis/harshad? n) (swap! message conj "harshad") (swap! message conj "!harshad"))
    (str/join "/" @message)))

(defn output [start end]
  (let [ran (range start end)]
    (->> (map vector ran (map reveal-characteristics ran))
         (map #(str/join "," %)))))

;; (defn oo [start end]
;;   (doseq [x (output start end)]
;;     (println x)))

(def a (filter queen/semiprime? (range 1 100)))
(def b (filter queen/square-free? (range 1 100)))
(clojure.set/difference (set a) (set b))

(doseq [x (output 1 200)]
  (println x))