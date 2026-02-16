(ns strudel-overtone.util-test
  (:require  [clojure.test :as t]
             [strudel-overtone.strudel-overtone :as sut]))

(t/deftest test-cycle-n
  (t/is (= [1 2 1 2 1] (sut/cycle-n 5 [1 2]))))
