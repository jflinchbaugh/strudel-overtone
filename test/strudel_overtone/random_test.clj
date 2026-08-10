(ns strudel-overtone.random-test
  (:require [clojure.test :refer [deftest testing is]]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest trigger-event-with-streams-test
  (testing "trigger-event resolves stream functions for numeric params"
    (let [mock-calls (atom [])]
      (with-redefs [ov/apply-at (fn [& _] (swap! mock-calls conj :log-called))
                    player/at-metro (fn [beat synth-var args]
                                      (swap! mock-calls conj {:beat beat :args (apply hash-map args)}))
                    synths/resolve-synth (constantly (fn [& _] nil))
                    ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)]
        (sut/seed! 0)
        (let [ev (sut/->Event 0 1 {:sound "saw" :amp (sut/srand 0.5 0.6)})]
          (sut/trigger-event :test-key ev 10.0 1)
          (let [args (:args (second @mock-calls))
                amp (:amp args)]
            (is (>= amp 0.5))
            (is (<= amp 0.6)))))))

  (testing "trigger-event resolves stream functions for sound param"
    (let [mock-calls (atom [])]
      (with-redefs [ov/apply-at (fn [& _] (swap! mock-calls conj :log-called))
                    player/at-metro (fn [beat synth-var args]
                                      (swap! mock-calls conj {:beat beat :synth synth-var}))
                    synths/resolve-synth (fn [s] (when (= s "kick") (fn [& _] nil)))
                    ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)]
        (sut/seed! 0)
        ;; choose that returns "kick"
        (let [ev (sut/->Event 0 1 {:sound (sut/choose ["kick" "kick"])})]
          (sut/trigger-event :test-key ev 10.0 1)
          (let [synth (:synth (second @mock-calls))]
            (is (fn? synth))))))))
