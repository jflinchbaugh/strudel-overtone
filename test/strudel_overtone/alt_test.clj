(ns strudel-overtone.alt-test
  (:require [clojure.test :refer [deftest is testing]]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.player :as player]
            [overtone.core :as ov]))

(deftest alt-test
  (testing "alt alternates values based on *current-cycle*"
    (let [a (p/alt :kick :snare :hat)]
      (binding [p/*current-cycle* 0]
        (is (= :kick (a 0 :sound))))
      (binding [p/*current-cycle* 1]
        (is (= :snare (a 0 :sound))))
      (binding [p/*current-cycle* 2]
        (is (= :hat (a 0 :sound))))
      (binding [p/*current-cycle* 3]
        (is (= :kick (a 0 :sound))))))

  (testing "alt works within a pattern"
    (let [pat (p/s [:bd (p/alt :sd :cp)])
          evs (:events pat)
          ev1 (first evs)
          ev2 (second evs)
          res-params (fn [ev cycle]
                       (player/resolve-params (:params ev) (:time ev) cycle))]
      (is (= :bd (:sound (res-params ev1 0))))
      (is (= :sd (:sound (res-params ev2 0))))
      (is (= :bd (:sound (res-params ev1 1))))
      (is (= :cp (:sound (res-params ev2 1)))))))

(deftest slowcat-test
  (testing "slowcat concatenates patterns"
    (let [p1 (p/s [:bd :sd])
          p2 (p/s [:hh :hh :hh :hh])
          combined (p/slowcat p1 p2)
          events (:events combined)]
      (is (= 6 (count events)))
      (is (== 2.0 (:length combined)))
      ;; First pattern events
      (is (= 0.0 (:time (nth events 0))))
      (is (= 0.5 (:time (nth events 1))))
      ;; Second pattern events (offset by 1 cycle)
      (is (= 1.0 (:time (nth events 2))))
      (is (= 1.25 (:time (nth events 3)))))))

(deftest stack-test
  (testing "stack overlays patterns"
    (let [p1 (p/s [:bd :sd])
          p2 (p/s [:hh :hh])
          combined (p/stack p1 p2)
          events (:events combined)]
      (is (= 4 (count events)))
      (is (== 1.0 (:length combined)))
      (is (= 0.0 (:time (nth events 0))))
      (is (= 0.0 (:time (nth events 2)))))))

(deftest fastcat-test
  (testing "fastcat squeezes patterns into one cycle"
    (let [p1 (p/s [:bd :sd])
          p2 (p/s [:hh :hh])
          combined (p/fastcat p1 p2)
          events (:events combined)]
      (is (= 4 (count events)))
      (is (= 1.0 (:length combined)))
      ;; First pattern is in [0, 0.5]
      (is (= 0.0 (:time (nth events 0))))
      (is (= 0.25 (:time (nth events 1))))
      ;; Second pattern is in [0.5, 1.0]
      (is (= 0.5 (:time (nth events 2))))
      (is (= 0.75 (:time (nth events 3)))))))
