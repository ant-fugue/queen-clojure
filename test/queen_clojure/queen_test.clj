(ns queen-clojure.queen-test
  (:require [clojure.test :refer :all]
            [queen-clojure.queen :refer :all]))

(deftest queen-test
  (testing "binary->dicimal"
    (is (= 0 (binary->dicimal "0")))
    (is (= 1 (binary->dicimal "1")))
    (is (= 0 (binary->dicimal "0000")))
    (is (= 1 (binary->dicimal "0001")))
    (is (= 15 (binary->dicimal "1111"))))

  (testing "digit-sum"
    (is (= 0 (digit-sum 0)))
    (is (= 1 (digit-sum 1)))
    (is (= 1 (digit-sum 10)))
    (is (= 15 (digit-sum 12345))))

  (testing "factorial"
    (is (= 1 (factorial 0)))
    (is (= 1 (factorial 1)))
    (is (= 120 (factorial 5))))

  (testing "factorials-i"
    (is (= '(1) (factorials-i 0)))
    (is (= '(1 1) (factorials-i 1)))
    (is (= '(1 1 2 6 24 120) (factorials-i 5))))

  (testing "distinct-int-pairs"
    (is (= '() (distinct-int-pairs 0)))
    (is (= '() (distinct-int-pairs 1)))
    (is (= '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3)) (distinct-int-pairs 4))))

;; (testing "pytagorean-int-trios"
;;   (is (= '() (pytagorean-int-trios 0)))
;;   (is (= '() (pytagorean-int-trios 1)))
;;   (is (= '([2 1 5] [3 1 10] [3 2 13] [4 1 17] [4 2 20] [4 3 25]) (pytagorean-int-trios 4))))

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

  (testing "div-group"
    (is (= "deficient" (div-group 2)))
    (is (= "perfect" (div-group 6)))
    (is (= "abundant" (div-group 12))))

  (testing "coprime?"
    (is (= true (coprime? 36 5)))
    (is (= true (coprime? 1 1)))
    (is (= false (coprime? 92 23))))

;; (testing "perfect-power-under"
;;   (is (= [1 4 8 9 16 25 27 32 36 49 64 81] (perfect-power-under 100))))


  (testing "prime?"
    (is (= true (prime? 2)))
    (is (= true (prime? 3)))
    (is (= false (prime? 6)))
    (is (= false (prime? 1))))

  (testing "composite?"
    (is (= false (composite? 2)))
    (is (= false (composite? 3)))
    (is (= true (composite? 6)))
    (is (= false (composite? 1))))

  (testing "prime-factors"
    (is (= [] (prime-factors 0)))
    (is (= [] (prime-factors 1)))
    (is (= [2] (prime-factors 2)))
    (is (= [2 2 2 5 5 5] (prime-factors 1000))))

  (testing "semiprime?"
    (is (= [4 6 9 10 14 15 21 22 25 26] (semi-prime-under 30))))

  (testing "square-free?"
    (is (= false (square-free? 0)))
    (is (= true (square-free? 1)))
    (is (= true (square-free? 2)))
    (is (= false (square-free? 1000))))

  (testing "square-free-numbers-under"
    (is (= [] (square-free-numbers-under 0)))
    (is (= [] (square-free-numbers-under 1)))
    (is (= [1 2 3 5 6 7 10 11 13 14 15 17 19] (square-free-numbers-under 20))))

  (testing "primes-seq"
    (is (= '() (prime-seq 0)))
    (is (= '() (prime-seq 1)))
    (is (= '(2 3 5) (prime-seq 5)))
    (is (= '(5 7) (prime-seq 5 10))))

  (testing "composites-under"
    (is (= '() (composites-under 0)))
    (is (= '() (composites-under 1)))
    (is (= '(4) (composites-under 5))))

  (testing "highly-composite-under"
    (is (= [1 2 4 6 12 24 36 48 60] (highly-composite-under 100))))

  (testing "derange"
    (is (= 1 (derange 0)))
    (is (= 0 (derange 1)))
    (is (= 1 (derange 2)))
    (is (= 2 (derange 3))))

  (testing "derangements-under"
    (is (= [1 0 1 2 9 44 265] (derangements-under 6))))

  (testing "prime-gaps"
    (is (= [1 2 2 4 2 4 2 4 6 2 6 4 2 4] (prime-gaps 50))))

  (testing "fib"
    (is (= [0 1 1 2 3 5 8 13 21 34] (map fib (range 0 10)))))

  (testing "fib-seq-under"
    (is (= [0 1 1 2 3 5 8 13] (fib-seq-under 20))))

  (testing "zechkendorf-exp"
    (is (=  '([] [1] [2] [3] [1 3] [5] [1 5] [2 5] [8] [1 8] [2 8]) (map zeckendorf-exp (range 0 11)))))


  (testing "lucas"
    (is (= [2 1 3 4 7 11 18 29 47 76] (map lucas (range 0 10)))))

  (testing "tribonacci"
    (is (= [0 0 1 1 2 4 7 13 24 44] (map tribonacci (range 0 10)))))

  (testing "kaprekar-routine"
    (is (= 0 (kaprekar-routine 0)))
    (is (= 0 (kaprekar-routine 333)))
    (is (= 495 (kaprekar-routine 495)))
    (is (= 198 (kaprekar-routine 324))))

  (testing "kaprekar-routine-seq"
    (is (= [0] (kaprekar-routine-seq 0)))
    (is (= [0] (kaprekar-routine-seq 333)))
    (is (= [495] (kaprekar-routine-seq 495)))
    (is (= [198 792 693 594 495] (kaprekar-routine-seq 324)))))
