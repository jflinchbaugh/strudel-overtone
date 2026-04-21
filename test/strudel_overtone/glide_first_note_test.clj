(ns strudel-overtone.glide-first-note-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest glide-first-note-logic-test
  (testing "first note of a mono synth should NOT have a glide from the previous note of a DIFFERENT loop"
    (let [player-state (atom {:playing? true :patterns {} :loops #{} :last-freq {}})
          at-metro-calls (atom [])]
      (with-redefs [player/player-state player-state
                    ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [_ f] (f))
                    synths/resolve-synth (constantly (fn [& _] nil))
                    player/at-metro-mono (fn [beat key voice-idx synth-var args]
                                           (swap! at-metro-calls conj (apply hash-map args)))]

        ;; 1. Play loop :p1 with a note to set a 'last-freq' in some other context (simulated)
        (swap! player-state assoc-in [:last-freq [:p1 0]] 1000.0)

        ;; 2. Trigger FIRST note of loop :p2 (which has no last-freq)
        ;; Even if it's monophonic, it should start at its own frequency, not 1000.0
        (let [ev {:params {:note :c3 :sound :saw :monophonic 1 :slide 0.1}}
              target-freq (ov/midi->hz (ov/note :c3))]
          (player/trigger-event :p2 ev 0 1 0)
          
          (let [args (first @at-metro-calls)]
            (is (= target-freq (:slide-from args)) 
                "First note should slide from its own frequency, even in mono mode"))))))

  (testing "polyphonic synth should NOT have a glide even if last-freq exists"
    (let [player-state (atom {:playing? true :patterns {} :loops #{} :last-freq {}})
          at-metro-calls (atom [])]
      (with-redefs [player/player-state player-state
                    ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [_ f] (f))
                    synths/resolve-synth (constantly (fn [& _] nil))
                    player/at-metro (fn [beat synth-var args]
                                      (swap! at-metro-calls conj (apply hash-map args)))]

        ;; 1. Set a 'last-freq' for this loop/voice
        (swap! player-state assoc-in [:last-freq [:p1 0]] 1000.0)

        ;; 2. Trigger note of loop :p1 in POLYPHONIC mode
        (let [ev {:params {:note :c3 :sound :saw :monophonic 0 :slide 0.1}}
              target-freq (ov/midi->hz (ov/note :c3))]
          (player/trigger-event :p1 ev 0 1 0)
          
          (let [args (first @at-metro-calls)]
            (is (= target-freq (:slide-from args)) 
                "Polyphonic note should never glide from previous freq")))))))
