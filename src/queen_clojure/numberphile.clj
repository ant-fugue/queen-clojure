(ns queen-clojure.numberphile
  (:require
   [queen-clojure.queen :as queen]))

(mod 25 24)
(->> (queen/prime-seq 100)
     (map #(Math/pow % 2))
     (map int)
     (map #(mod % 24)))
(->> (range 1 20)
     (map #(Math/pow % 2))
     (map int)
     (map #(mod % 24)))
(map #(mod % 24) (queen/prime-seq 100))

(defn ff [a b]
  (if (= (* a b) (+ a a b b))
    true
    false))

(ff 3 5)
(ff 3 6)
