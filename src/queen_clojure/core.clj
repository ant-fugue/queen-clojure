(ns queen-clojure.core
  (:gen-class))


(defn odd-sec [num] (filter odd? (range 0 num)))

;; (defn cullen-numbers [num] (map (fn [n] (* n (m/pow 2 n))) (range 0 num)))





(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (+ 2 3))

  (println "Hello, World!"))
