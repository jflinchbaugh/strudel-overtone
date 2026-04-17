(ns strudel-overtone.stop-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest test-stop-deferred-gating
  (testing "stop! [key] should only remove from :loops and let play-loop handle gating"
    (let [gate-off-calls (atom [])
          player-state (atom {:playing? true
                             :loops #{:p1}
                             :patterns {:p1 {}}
                             :active-synths {[:p1 0] {:inst {:id 101} :synth :saw}}})]
      (with-redefs [sut/player-state player-state
                    sut/gate-off (fn [inst] (swap! gate-off-calls conj inst))
                    sut/metro (constantly 0)
                    ov/apply-by (fn [& _] nil)]

        ;; 1. Call stop!
        (sut/stop! :p1)

        ;; Should be removed from :loops but NOT gated off yet
        (is (not (contains? (:loops @player-state) :p1)))
        (is (empty? @gate-off-calls) "Should not gate off immediately")
        (is (contains? (:active-synths @player-state) [:p1 0])
          "Active synth should still be there")

        ;; 2. Simulate play-loop running its final iteration (the one that was already scheduled)
        (sut/play-loop :p1 0)

        ;; Now it should be gated off
        (is (= 1 (count @gate-off-calls)))
        (is (= 101 (:id (first @gate-off-calls))))
        (is (empty? (:active-synths @player-state))
          "Active synth should be cleaned up"))))

  (testing "stop! [] should set :playing? false and let play-loops handle gating"
    (let [gate-off-calls (atom [])
          player-state (atom {:playing? true
                             :loops #{:p1}
                             :patterns {:p1 {}}
                             :active-synths {[:p1 0] {:inst {:id 101} :synth :saw}}})]
      (with-redefs [sut/player-state player-state
                    sut/gate-off (fn [inst] (swap! gate-off-calls conj inst))
                    sut/metro (constantly 0)
                    ov/apply-by (fn [& _] nil)]

        (sut/stop!)

        (is (false? (:playing? @player-state)))
        (is (empty? @gate-off-calls) "Should not gate off immediately")

        (sut/play-loop :p1 0)

        (is (= 1 (count @gate-off-calls)))
        (is (empty? (:active-synths @player-state)))))))
