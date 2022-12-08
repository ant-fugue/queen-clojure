(ns queen-clojure.queen-test
  (:require [clojure.test :refer :all]
            [queen-clojure.queen :refer :all]))

(testing "factorial"
  (is (= 1 (factorial 0)))
  (is (= 1 (factorial 1)))
  (is (= 120 (factorial 5))))
