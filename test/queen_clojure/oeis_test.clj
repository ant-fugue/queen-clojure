(ns queen-clojure.oeis-test
  (:require [clojure.test :refer :all]
            [queen-clojure.oeis :refer :all]))

(testing "lower-wythoff-seq-i"
  (is (= [1 3 4 6 8] (lower-wythoff-seq-i 5))))

(testing "cullen-seq-i"
  (is (= [1 3 9 25 65] (cullen-seq-i 5))))

(testing "A002310-seq-i"
  (is (= [1 2 9 43 206] (A002310-seq-i 5))))

(testing "alt-fact"
  (is (= nil (alt-fact 0)))
  (is (= 1 (alt-fact 1)))
  (is (= 1 (alt-fact 2)))
  (is (= 101 (alt-fact 5))))


(testing "digital-root"
  (is (= 0 (digital-root 0)))
  (is (= 1 (digital-root 1)))
  (is (= 6 (digital-root 132189))))

(testing "A097285-seq-under"
  (is (= [1 2 1 3 2 3 1 4 2 4 3 4] (A097285-seq-under 4))))

(testing "power-sum-dig?"
  (is (= true (power-sum-dig? 0)))
  (is (= false (power-sum-dig? 10)))
  (is (= true (power-sum-dig? 512)))
  (is (= false (power-sum-dig? 321))))

(testing "power-sum-seq-under"
  (is (= [0 1 2 3 4 5 6 7 8 9 81 512 2401 4913 5832] (power-sum-seq-under 10000))))

(testing "sohpie-germain-p-under"
  (is (= [2 3 5 11 23 29 41 53 83 89 113 131 173 179 191] (sophie-germain-p-under 200))))