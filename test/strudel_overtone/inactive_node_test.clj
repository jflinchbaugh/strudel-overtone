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

(deftest at-metro-mono-node-status-test
  (testing "at-metro-mono starts new inst if old one is inactive"
    (let [start-called (atom false)
          update-called (atom false)
          mock-inst {:id 101}
          player-state (atom {:playing? true
                             :loops #{:p1}
                             :active-synths {[:p1 0] {:inst mock-inst :synth :saw}}})]
      (with-redefs [sut/player-state player-state
                    sut/metro (constantly 0)
                    ov/apply-at (fn [_ f] (f))
                    ov/node-active? (constantly false)
                    sut/start-mono-inst (fn [& _] (reset! start-called true))
                    sut/update-mono-inst (fn [& _] (reset! update-called true))]
        (sut/at-metro-mono 0 :p1 0 :saw [])
        (is (true? @start-called))
        (is (false? @update-called)))))

  (testing "at-metro-mono updates existing inst if it is active"
    (let [start-called (atom false)
          update-called (atom false)
          mock-inst {:id 101}
          player-state (atom {:playing? true
                             :loops #{:p1}
                             :active-synths {[:p1 0] {:inst mock-inst :synth :saw}}})]
      (with-redefs [sut/player-state player-state
                    sut/metro (constantly 0)
                    ov/apply-at (fn [_ f] (f))
                    ov/node-active? (constantly true)
                    sut/start-mono-inst (fn [& _] (reset! start-called true))
                    sut/update-mono-inst (fn [& _] (reset! update-called true))]
        (sut/at-metro-mono 0 :p1 0 :saw [])
        (is (false? @start-called))
        (is (true? @update-called))))))
