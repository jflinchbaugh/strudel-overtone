(ns strudel-overtone.stop-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [overtone.core :as ov]))

(deftest test-stop-immediate-gating
  (testing "stop! [key] should gate off synths immediately"
    (let [gate-off-calls (atom [])
          player-state (atom {:playing? true
                             :loops #{:p1}
                             :patterns {:p1 {}}
                             :active-synths {[:p1 0] {:inst {:id 101} :synth :saw}}})]
      (with-redefs [player/player-state player-state
                    player/gate-off (fn [inst] (swap! gate-off-calls conj inst))
                    player/metro (constantly 0)
                    ov/apply-by (fn [& _] nil)]

        ;; 1. Call stop!
        (sut/stop! :p1)

        ;; Should be removed from :loops and gated off immediately
        (is (not (contains? (:loops @player-state) :p1)))
        (is (= 1 (count @gate-off-calls)))
        (is (= 101 (:id (first @gate-off-calls))))
        (is (empty? (:active-synths @player-state))
          "Active synth should be cleaned up")

        ;; 2. Simulate play-loop running its final iteration (the one that was already scheduled)
        (sut/play-loop :p1 0 0)

        ;; Now it should be gated off (already was, but calling it again should be safe/no-op)
        (is (= 1 (count @gate-off-calls))))))

  (testing "stop! [] should set :playing? false and patterns/loops to empty"
    (let [gate-off-calls (atom [])
          player-state (atom {:playing? true
                             :loops #{:p1}
                             :patterns {:p1 {}}
                             :active-synths {[:p1 0] {:inst {:id 101} :synth :saw}}})]
      (with-redefs [player/player-state player-state
                    player/gate-off (fn [inst] (swap! gate-off-calls conj inst))
                    player/metro (constantly 0)
                    ov/apply-by (fn [& _] nil)]

        (sut/stop!)

        (is (false? (:playing? @player-state)))
        (is (empty? (:loops @player-state)))
        (is (empty? (:patterns @player-state)))
        (is (= 1 (count @gate-off-calls)))
        (is (= 101 (:id (first @gate-off-calls))))
        (is (empty? (:active-synths @player-state)))))))

(deftest test-stop-resets-metro
  (testing "stop! [] resets metronome to beat 0"
    (let [metro-calls (atom [])
          player-state (atom {:playing? true
                             :loops #{:p1}
                             :patterns {:p1 {}}
                             :active-synths {}})]
      (with-redefs [player/player-state player-state
                    player/gate-off (constantly nil)
                    player/metro (fn [& args]
                                   (swap! metro-calls conj args)
                                   0)]
        (sut/stop!)
        (is (some #(= [0] %) @metro-calls))))))

(deftest test-play-resets-metro-when-idle
  (testing "play! resets metronome to beat 0 if nothing was playing"
    (let [metro-calls (atom [])
          player-state (atom {:playing? false
                             :loops #{}
                             :patterns {}
                             :active-synths {}})]
      (with-redefs [player/player-state player-state
                    player/metro (fn [& args]
                                   (swap! metro-calls conj args)
                                   0)
                    ov/apply-by (fn [& _] nil)]
        (sut/play! :drums (sut/s [:kick]))
        (is (some #(= [0] %) @metro-calls))))))

