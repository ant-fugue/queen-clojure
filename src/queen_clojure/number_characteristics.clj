(ns queen-clojure.number_characteristics
  (:require [clojure.string :as str]
            [queen-clojure.queen :as queen]))

(defn reveal-characteristics [n]
  (let [message (atom [])]
    (when (queen/prime? n) (swap! message conj "prime/"))
    (when (queen/square-free? n) (swap! message conj "square free/"))))

(map reveal-characteristics (range 1 10))
