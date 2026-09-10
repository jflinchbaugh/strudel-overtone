(ns strudel-overtone.random-test
  (:require [clojure.test :refer [deftest testing is]]
            [strudel-overtone.core :as sut]
            [strudel-overtone.pattern :as p]
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

(deftest sometimes-test
  (testing "sometimes applies transform probabilistically per cycle"
    (sut/seed! 42)
    (let [base (sut/s [:bd :sd])
          pat (-> base
                  (sut/sometimes sut/rev))
          evs (:events pat)
          res-params (fn [ev cycle]
                       (player/resolve-params
                        (:params ev) (:time ev) cycle))
          active-sounds (fn [cycle]
                          (binding [p/*current-cycle* cycle]
                            (->> evs
                                 (filter (fn [e]
                                           (not= 0 (get (res-params e cycle)
                                                        :active 1))))
                                 (sort-by :time)
                                 (mapv (fn [e]
                                         (:sound (res-params e cycle)))))))]
      ;; Test over several cycles to ensure both normal and reversed occur
      (let [cycles-output (mapv active-sounds (range 10))]
        (is (some #(= [:sd :bd] %) cycles-output)
            "should sometimes reverse")
        (is (some #(= [:bd :sd] %) cycles-output)
            "should sometimes remain unreversed"))))

  (testing "sometimes supports (sometimes pat f) and (sometimes f pat)"
    (sut/seed! 42)
    (let [base (sut/s [:bd :sd])
          pat1 (sut/sometimes base sut/rev)
          pat2 (sut/sometimes sut/rev base)
          pat3 (-> base (sut/sometimes sut/rev))]
      (is (= (count (:events pat1)) (count (:events pat2))))
      (is (= (count (:events pat1)) (count (:events pat3)))))))
