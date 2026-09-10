(ns strudel-overtone.random-test
  (:require [clojure.test :refer [deftest testing is]]
            [strudel-overtone.core :as sut]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov])
  (:import [strudel_overtone.pattern Pattern]))

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
      (is (= (count (:events pat1)) (count (:events pat3))))))

  (testing "statements after sometimes in a thread apply to both branches"
    (sut/seed! 42)
    (let [base (sut/s [:bd :sd])
          pat (-> base
                  (sut/sometimes sut/rev)
                  (sut/gain 0.4))
          evs (:events pat)
          res-params (fn [ev cycle]
                       (player/resolve-params
                        (:params ev) (:time ev) cycle))
          active-amps (fn [cycle]
                        (binding [p/*current-cycle* cycle]
                          (->> evs
                               (filter (fn [e]
                                         (not= 0 (get (res-params e cycle)
                                                      :active 1))))
                               (mapv (fn [e]
                                       (:amp (res-params e cycle)))))))]
      (doseq [cycle (range 10)]
        (is (= [0.4 0.4] (active-amps cycle))))))

  (testing "degrade after sometimes keeps remaining events with correct gain"
    (sut/seed! 42)
    (let [base (sut/s [:bd :sd :hh :cp])
          pat (-> base
                  (sut/sometimes sut/rev)
                  (sut/degrade 0.25)
                  (sut/gain 0.4))
          evs (:events pat)
          res-params (fn [ev cycle]
                       (player/resolve-params
                        (:params ev) (:time ev) cycle))
          active-amps (fn [cycle]
                        (binding [p/*current-cycle* cycle]
                          (->> evs
                               (filter (fn [e]
                                         (not= 0 (get (res-params e cycle)
                                                      :active 1))))
                               (mapv (fn [e]
                                       (:amp (res-params e cycle)))))))]
      (doseq [cycle (range 10)]
        (let [amps (active-amps cycle)]
          (is (seq amps) "should have active events")
          (is (every? #(= 0.4 %) amps)
              "all active events should have gain 0.4"))))))

(deftest degrade-test
  (testing "degrade drops events probabilistically per cycle"
    (sut/seed! 42)
    (let [pat (-> (sut/s [:bd :sd :hh :cp])
                  (sut/degrade 0.5))
          evs (:events pat)
          res-params (fn [ev cycle]
                       (player/resolve-params
                        (:params ev) (:time ev) cycle))
          active-count (fn [cycle]
                         (binding [p/*current-cycle* cycle]
                           (->> evs
                                (filter (fn [e]
                                          (not= 0 (get (res-params e cycle)
                                                       :active 1))))
                                count)))]
      (let [counts (mapv active-count (range 10))]
        (is (some #(< % 4) counts) "should drop some events")
        (is (some #(> % 0) counts) "should keep some events"))))

  (testing "degrade curried form (degrade p)"
    (sut/seed! 42)
    (let [deg-fn (sut/degrade 0.25)]
      (is (fn? deg-fn))
      (let [pat (deg-fn (sut/s [:bd :sd]))]
        (is (= 2 (count (:events pat))))))))

(deftest curried-transforms-test
  (testing "curried fast and slow work with sometimes"
    (let [fast-fn (sut/fast 2)
          slow-fn (sut/slow 2)]
      (is (fn? fast-fn))
      (is (fn? slow-fn))
      (let [pat (-> (sut/s [:bd :sd])
                    (sut/sometimes (sut/fast 2)))]
        (is (instance? Pattern pat)))))

  (testing "curried add, pan, gain, lpf work with sometimes"
    (let [add-fn (sut/add 12)
          pan-fn (sut/pan -1)
          gain-fn (sut/gain 0.2)
          lpf-fn (sut/lpf 800)]
      (is (fn? add-fn))
      (is (fn? pan-fn))
      (is (fn? gain-fn))
      (is (fn? lpf-fn))
      (let [pat (-> (sut/note [:c4 :eb4])
                    (sut/sometimes (sut/add 12))
                    (sut/sometimes (sut/pan -1))
                    (sut/sometimes (sut/lpf 800)))]
        (is (instance? Pattern pat)))))

  (testing "curried echo, early, late work with sometimes"
    (let [echo-fn (sut/echo 0.25 0.5)
          early-fn (sut/early 0.25)
          late-fn (sut/late 0.25)]
      (is (fn? echo-fn))
      (is (fn? early-fn))
      (is (fn? late-fn))
      (let [pat (-> (sut/s [:bd :sd])
                    (sut/sometimes (sut/echo 0.25 0.5))
                    (sut/sometimes (sut/early 0.25)))]
        (is (instance? Pattern pat))))))
