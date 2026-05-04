(ns strudel-overtone.degrees-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.player :as player]
            [overtone.core :as ov]))

(deftest degrees-test
  (testing "degrees function correctly maps 1-indexed integers to midi notes"
    (let [pat (-> (p/note :c4)
                  (p/degrees :major [1 3 5 8]))
          events (:events pat)
          notes (map (fn [e]
                       (let [res (player/resolve-params (:params e) 0 0)
                             n-raw (:note res)
                             degree (get res :degree 0)]
                         (if (number? n-raw)
                           (+ n-raw degree)
                           (+ (ov/note n-raw) degree))))
                     events)]
      (is (= [60 64 67 72] (vec (map long notes)))))))
