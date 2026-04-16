(ns strudel-overtone.inactive-node-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest gate-off-inactive-node-test
  (testing "gate-off does not throw when node is inactive"
    (let [mock-inst {:id 66}
          ctl-called (atom false)]
      (with-redefs [ov/node-active? (constantly false)
                    ov/ctl (fn [& _] (reset! ctl-called true))]
        ;; This should not throw and should not call ctl
        (is (nil? (@#'sut/gate-off mock-inst)))
        (is (false? @ctl-called))))

  (testing "gate-off calls ctl when node is active"
    (let [mock-inst {:id 66}
          ctl-called (atom false)]
      (with-redefs [ov/node-active? (constantly true)
                    ov/ctl (fn [inst k v] 
                             (is (= :gate k))
                             (is (= 0 v))
                             (reset! ctl-called true))]
        (is (some? (@#'sut/gate-off mock-inst)))
        (is (true? @ctl-called)))))

  (testing "gate-off catches exceptions from ctl"
    (let [mock-inst {:id 66}]
      (with-redefs [ov/node-active? (constantly true)
                    ov/ctl (fn [& _] (throw (Exception. "Inactive node modification attempted")))]
        ;; This should catch the exception and return nil instead of crashing
        (is (nil? (@#'sut/gate-off mock-inst))))))))

(deftest stop-handles-inactive-nodes-test
  (testing "stop! cleans up state even if nodes are inactive"
    (let [mock-inst {:id 66}
          player-state (atom {:playing? true 
                             :patterns {:p1 {}} 
                             :loops #{:p1} 
                             :active-synths {:p1 {:inst mock-inst}}})]
      (with-redefs [sut/player-state player-state
                    ov/node-active? (constantly false)
                    ov/ctl (fn [& _] (throw (Exception. "Should not be called")))]
        (sut/stop!)
        (is (false? (:playing? @player-state)))
        (is (empty? (:active-synths @player-state)))
        (is (empty? (:loops @player-state)))))))
