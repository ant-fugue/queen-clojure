(ns queen-clojure.oeis-test
  (:require [clojure.test :refer :all]
            [queen-clojure.oeis :refer :all]))

(testing "lower-wythoff-seq-i"
  (is (= [1 3 4 6 8] (lower-wythoff-seq-i 5))))

(testing "cullen-seq-i"
  (is (= '(1 3 9 25 65) (cullen-seq-i 5))))

(testing "alt-fact"
  (is (= nil (alt-fact 0)))
  (is (= 1 (alt-fact 1)))
  (is (= 1 (alt-fact 2)))
  (is (= 101 (alt-fact 5))))