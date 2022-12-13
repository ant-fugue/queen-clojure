(ns queen-clojure.queen-test
  (:require [clojure.test :refer :all]
            [queen-clojure.queen :refer :all]))

(testing "factorial"
  (is (= 1 (factorial 0)))
  (is (= 1 (factorial 1)))
  (is (= 120 (factorial 5))))

(testing "factorials-i"
  (is (= '(1) (factorials-i 0)))
  (is (= '(1 1) (factorials-i 1)))
  (is (= '(1 1 2 6 24 120) (factorials-i 5))))

(testing "div-of"
  (is (= [] (div-of 0)))
  (is (= [1] (div-of 1)))
  (is (= [1 2 3 4 5 6 10 12 15 20 30 60] (div-of 60))))

(testing "num-of-div"
  (is (= 0 (num-of-div 0)))
  (is (= 2 (num-of-div 2)))
  (is (= 4 (num-of-div 6))))

(testing "div-sum"
  (is (= 0 (div-sum 0)))
  (is (= 1 (div-sum 1)))
  (is (= 12 (div-sum 6))))

(testing "div-of-except-self"
  (is (= [] (div-of-except-self 0)))
  (is (= [] (div-of-except-self 1)))
  (is (= [1 2 3 4 5 6 10 12 15 20 30] (div-of-except-self 60))))

(testing "num-of-div-except-self"
  (is (= 0 (num-of-div-except-self 0)))
  (is (= 1 (num-of-div-except-self 2)))
  (is (= 3 (num-of-div-except-self 6))))

(testing "div-sum-except-self"
  (is (= 0 (div-sum-except-self 0)))
  (is (= 0 (div-sum-except-self 1)))
  (is (= 6 (div-sum-except-self 6))))

(testing "perfect?"
  (is (= false (perfect? 0)))
  (is (= false (perfect? 1)))
  (is (= true (perfect? 28)))
  (is (= false (perfect? 7))))

(testing "deficient?"
  (is (= false (deficient? 0)))
  (is (= true (deficient? 1)))
  (is (= true (deficient? 17)))
  (is (= false (deficient? 28))))

(testing "abundant?"
  (is (= false (abundant? 0)))
  (is (= false (abundant? 1)))
  (is (= true (abundant? 12)))
  (is (= false  (abundant? 28))))

(testing "prime?"
  (is (= true (prime? 2)))
  (is (= true (prime? 3)))
  (is (= false (prime? 6)))
  (is (= false (prime? 1))))

(testing "prime-factors"
  (is (= [] (prime-factors 0)))
  (is (= [] (prime-factors 1)))
  (is (= [2] (prime-factors 2)))
  (is (= [2 2 2 5 5 5] (prime-factors 1000))))

(testing "square-free?"
  (is (= false (square-free? 0)))
  (is (= true (square-free? 1)))
  (is (= true (square-free? 2)))
  (is (= false (square-free? 1000))))

(testing "square-free-numbers-under"
  (is (= [] (square-free-numbers-under 0)))
  (is (= [1] (square-free-numbers-under 1)))
  (is (= [1, 2, 3, 5, 6, 7, 10, 11, 13, 14, 15, 17, 19] (square-free-numbers-under 20))))

(testing "primes-under"
  (is (= '() (primes-under 0)))
  (is (= '() (primes-under 1)))
  (is (= '(2 3 5) (primes-under 5))))

