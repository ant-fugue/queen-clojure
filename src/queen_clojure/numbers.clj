(ns queen-clojure.numbers
  (:require [clojure.string :as str]
            [queen-clojure.queen :as queen]
            [queen-clojure.oeis :as oeis]))

(defn reveal-characteristics [n]
  (let [message (atom [])]
    (when (queen/prime? n) (swap! message conj "prime"))
    (when (queen/square-free? n) (swap! message conj "square free"))
    (when (queen/deficient? n) (swap! message conj "deficient"))
    (when (queen/abundant? n) (swap! message conj "abundant"))
    (when (queen/perfect? n) (swap! message conj "perfect"))
    (when (oeis/sophie-germain-p? n) (swap! message conj "sophie germain prime"))
    (when (oeis/power-sum-dig? n) (swap! message conj "power of the sum of its digits"))
    (str/join "/" @message)))

(defn output [start end]
  (let [ran (range start end)]
    (->> (map vector ran (map reveal-characteristics ran))
         (map #(str/join "," %)))))

;; (defn oo [start end]
;;   (doseq [x (output start end)]
;;     (println x)))

(doseq [x (output 1 100)]
  (println x))