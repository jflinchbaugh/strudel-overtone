(ns strudel-overtone.playing-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest playing-test
  (testing "playing returns list of active loops"
    (reset! sut/player-state {:playing? true :patterns {} :loops #{:p1 :p2}})
    (let [current (sut/playing)]
      (is (= 2 (count current)))
      (is (some #(= :p1 %) current))
      (is (some #(= :p2 %) current))))

  (testing "playing returns nil when no loops are active"
    (reset! sut/player-state {:playing? true :patterns {} :loops #{}})
    (is (nil? (sut/playing)))))
