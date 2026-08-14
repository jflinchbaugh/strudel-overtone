(ns strudel-overtone.every-cycle-test
  (:require [clojure.test :refer [deftest is testing]]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.player :as player]))

(deftest every-cycle-val-test
  (testing "every-cycle as a parameter value alternating every N cycles"
    (let [val-fn (p/every-cycle 4 :sd :cp)]
      (binding [p/*current-cycle* 0]
        (is (= :sd (val-fn 0 :sound))))
      (binding [p/*current-cycle* 1]
        (is (= :cp (val-fn 0 :sound))))
      (binding [p/*current-cycle* 2]
        (is (= :cp (val-fn 0 :sound))))
      (binding [p/*current-cycle* 3]
        (is (= :cp (val-fn 0 :sound))))
      (binding [p/*current-cycle* 4]
        (is (= :sd (val-fn 0 :sound)))))))

(deftest every-cycle-pattern-test
  (testing "every-cycle transforms pattern every N cycles"
    (let [base (p/s [:bd :sd])
          pat (p/every-cycle base 4 p/rev)
          evs (:events pat)
          res-params (fn [ev cycle]
                       (player/resolve-params (:params ev) (:time ev) cycle))]
      ;; Events from both normal and transformed are present
      ;; but their :active status toggles with the cycle
      (binding [p/*current-cycle* 0]
        ;; On cycle 0 (every 4th cycle), rev version is active
        (let [active-evs (->> evs
                              (filter (fn [e]
                                        (not= 0 (get (res-params e 0) :active 1))))
                              (sort-by :time))]
          (is (= :sd (:sound (res-params (first active-evs) 0))))
          (is (= :bd (:sound (res-params (second active-evs) 0))))))

      (binding [p/*current-cycle* 1]
        ;; On cycle 1 (normal cycle), base version is active
        (let [active-evs (->> evs
                              (filter (fn [e]
                                        (not= 0 (get (res-params e 1) :active 1))))
                              (sort-by :time))]
          (is (= :bd (:sound (res-params (first active-evs) 1))))
          (is (= :sd (:sound (res-params (second active-evs) 1))))))))

  (testing "curried / threaded syntax support"
    (let [pat (-> (p/s [:bd :sd])
                  (p/every-cycle 4 p/rev))]
      (is (instance? strudel_overtone.pattern.Pattern pat)))))
