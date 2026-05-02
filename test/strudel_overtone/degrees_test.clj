(ns strudel-overtone.degrees-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.pattern :as p]))

(deftest degrees-test
  (testing "degrees function correctly maps 1-indexed integers to midi notes"
    (let [pat (-> (p/note :c4)
                  (p/degrees :major [1 3 5 8]))
          events (:events pat)
          notes (map (fn [e]
                       (let [n-fn (get-in e [:params :note])]
                         (n-fn 0 :note)))
                     events)]
      (is (= [60 64 67 72] (vec notes))))))
