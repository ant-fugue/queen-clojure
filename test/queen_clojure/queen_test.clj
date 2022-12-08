(ns queen-clojure.queen-test
  (:require [clojure.test :refer :all]
            [queen-clojure.queen :refer :all]))

(deftest factorial
  (testing "sample test"
    (is (= 1 (queen-clojure.queen/factorial 0)))))
