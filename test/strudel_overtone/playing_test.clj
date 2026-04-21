(ns strudel-overtone.playing-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [overtone.core :as ov]))

(deftest playing-test
  (testing "playing returns list of active loops"
    (with-redefs [player/player-state (atom {:playing? true :patterns {} :loops #{:p1 :p2}})]
      (let [current (sut/playing)]
        (is (= 2 (count current)))
        (is (some #(= :p1 %) current))
        (is (some #(= :p2 %) current)))))

  (testing "playing returns nil when no loops are active"
    (with-redefs [player/player-state (atom {:playing? true :patterns {} :loops #{}})]
      (is (nil? (sut/playing))))))

