(ns strudel-overtone.euclid-test
  (:require [clojure.test :refer [deftest is testing]]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.core :as sc]))

(deftest euclid-vector-test
  (testing "euclid rhythm generation with default hit and rest"
    ;; 3 hits over 8 steps -> [x - - x - - x -]
    (is (= [:x :- :- :x :- :- :x :-]
           (p/euclid 3 8)))
    ;; 5 hits over 8 steps -> [x - x - x x - x]
    (is (= [:x :- :x :- :x :x :- :x]
           (p/euclid 5 8)))
    ;; 4 hits over 12 steps -> [x - - x - - x - - x - -]
    (is (= [:x :- :- :x :- :- :x :- :- :x :- :-]
           (p/euclid 4 12)))
    ;; 2 hits over 5 steps -> [x - - x -]
    (is (= [:x :- :- :x :-]
           (p/euclid 2 5)))
    ;; 3 hits over 4 steps -> [x - x x]
    (is (= [:x :- :x :x]
           (p/euclid 3 4))))

  (testing "euclid with custom hit and rest values"
    (is (= [:bd :- :- :bd :- :- :bd :-]
           (p/euclid 3 8 :bd)))
    (is (= [:a4 :_ :_ :a4 :_ :_ :a4 :_]
           (p/euclid 3 8 :a4 :_))))

  (testing "euclid with rotation"
    ;; [x - - x - - x -] rotated by 1 -> [- x - - x - - x]
    (is (= [:- :x :- :- :x :- :- :x]
           (p/euclid 3 8 :x :- 1)))
    ;; rotated by 2
    (is (= [:x :- :x :- :- :x :- :-]
           (p/euclid 3 8 :x :- 2))))

  (testing "boundary conditions"
    (is (= [] (p/euclid 3 0)))
    (is (= [:- :- :- :-] (p/euclid 0 4 :x :-)))
    (is (= [:x :x :x :x] (p/euclid 4 4 :x :-)))
    (is (= [:x :x :x :x] (p/euclid 6 4 :x :-)))))

(deftest euclid-pattern-integration-test
  (testing "euclid can be used directly in (s ...) or (note ...)"
    (let [pat (p/s (p/euclid 3 8 :bd))
          evs (:events pat)
          active-evs (filter (fn [e]
                               (let [act (get-in e [:params :active] 1)]
                                 (not= 0 (if (fn? act) (act 0 :active) act))))
                             evs)]
      (is (= 8 (count evs)))
      (is (= 3 (count active-evs)))
      (is (= :bd (get-in (first active-evs) [:params :sound])))
      (is (= 0.0 (:time (first active-evs))))
      (is (= 0.375 (:time (second active-evs))))
      (is (= 0.75 (:time (nth active-evs 2)))))))
