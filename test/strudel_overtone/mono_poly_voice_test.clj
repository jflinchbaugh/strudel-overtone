(ns strudel-overtone.mono-poly-voice-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.player :as player]
            [strudel-overtone.pattern :as p]
            [overtone.core :as ov]
            [strudel-overtone.core :as sut]))

(deftest mono-poly-voice-test
  (testing "monophonic chords should handle voice count changes"
    (let [gate-calls (atom [])
          start-calls (atom [])
          player-state (atom {:playing? true :loops #{:p1} :patterns {} :active-synths {}})
          metro (fn [& _] 0)]
      (with-redefs [player/player-state player-state
                    player/gate-off (fn [inst] (swap! gate-calls conj inst))
                    player/start-mono-inst (fn [key vidx synth args old-inst]
                                             (swap! start-calls conj [key vidx args])
                                             (let [new-inst {:id (count @start-calls)}]
                                               (swap! player-state assoc-in [:active-synths [key vidx]] 
                                                      {:inst new-inst :synth synth})
                                               new-inst))
                    player/update-mono-inst (fn [inst args] 
                                              (swap! start-calls conj [:update (:id inst) args]))
                    player/metro metro
                    ov/metro-bpm (constantly 120)
                    ov/apply-at (fn [_ f] (f))
                    ov/at (fn [_ f] (f))
                    ov/note (fn [n] (if (keyword? n) 60 n))
                    ov/midi->hz (fn [n] n)
                    ov/node-active? (constantly true)]

        ;; 1. Play a chord of 3 notes monophonically
        (let [pat (-> (sut/note [#{:c3 :e3 :g3}]) (sut/mono))]
          (doseq [[vidx ev] (map-indexed vector (:events pat))]
            (player/trigger-event :p1 ev 0 1 vidx)))
        
        (is (= 3 (count (:active-synths @player-state))) "Should have 3 active synths")
        
        ;; 2. Play a single note monophonically
        (reset! start-calls [])
        (reset! gate-calls [])
        (let [pat (-> (sut/note [:c4]) (sut/mono))]
          (doseq [[vidx ev] (map-indexed vector (:events pat))]
            (player/trigger-event :p1 ev 4 1 vidx)))
        
        (is (= 1 (count (:active-synths @player-state))) "Should only have 1 active synth")
        (is (= 2 (count @gate-calls)) "Should have gated off the 2 extra voices")))))
