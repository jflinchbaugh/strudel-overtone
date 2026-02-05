(ns strudel-overtone.slice-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [strudel-overtone.strudel-overtone-test :as sut-test]
            [overtone.core :as ov]))

(deftest slice-sample-test
  (testing "slice-sample! creates virtual instruments"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    sut/at-metro (fn [beat synth-var args] (swap! mock-calls conj {:func synth-var :args [args]}))
                    sut/samples (atom {"break" {:id 1 :duration 4.0}})
                    sut/sample-slices (atom {})
                    sut/sampler-adsr (fn [& args] args)]

        (sut/slice-sample! :kick :break 0.0 0.1)
        (sut/slice-sample! :snare :break 0.5 0.6)

        (testing "triggering a slice uses source buffer and correct begin/end"
          (reset! mock-calls [])
          (let [ev (sut/->Event 0 1 {:sound "kick"})]
            (sut/trigger-event ev 0 1)

            (let [synth-call (second @mock-calls)
                  args (first (:args synth-call))
                  args-map (apply hash-map args)]
              (is (= 1 (:buf args-map)))
              (is (sut-test/approx= 0.0 (:begin args-map)))
              (is (sut-test/approx= 0.1 (:end args-map)))
              ;; total dur is 0.4. release is 0. sustain should be 0.4
              (is (sut-test/approx= 0.4 (:sustain args-map))))))

        (testing "triggering another slice"
          (reset! mock-calls [])
          (let [ev (sut/->Event 0 1 {:sound "snare"})]
            (sut/trigger-event ev 0 1)

            (let [synth-call (second @mock-calls)
                  args (first (:args synth-call))
                  args-map (apply hash-map args)]
              (is (= 1 (:buf args-map)))
              (is (sut-test/approx= 0.5 (:begin args-map)))
              (is (sut-test/approx= 0.6 (:end args-map))))))

        (testing "long slices are truncated to step duration"
          (reset! mock-calls [])
          (sut/slice-sample! :long-slice :break 0.0 0.5) ; 0.5 * 4.0 = 2.0s
          ;; step duration is 1 beat = 0.5s
          (let [ev (sut/->Event 0 0.25 {:sound "long-slice"})]
            (sut/trigger-event ev 0 1) ; dur-beats = 1
            (let [synth-call (second @mock-calls)
                  args (first (:args synth-call))
                  args-map (apply hash-map args)]
              (is (sut-test/approx= 0.5 (:sustain args-map))))))))))