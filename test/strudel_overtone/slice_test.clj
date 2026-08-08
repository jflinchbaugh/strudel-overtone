(ns strudel-overtone.slice-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.core-test :as sut-test]
            [strudel-overtone.player :as player]
            [strudel-overtone.samples :as samples]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest slice-sample-test
  (testing "slice-sample! creates virtual instruments"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    player/at-metro (fn [beat synth-var args] (swap! mock-calls conj {:func synth-var :args [args]}))
                    samples/samples (atom {:break {:id 1 :duration 4.0}})
                    samples/sample-slices (atom {})
                    synths/sampler (fn [& args] args)]

        (sut/slice-sample! :kick :break 0.0 0.1)
        (sut/slice-sample! :snare :break 0.5 0.6)

        (testing "triggering a slice uses source buffer and correct begin/end"
          (reset! mock-calls [])
          (let [ev (sut/->Event 0 1 {:sound :kick})]
            (sut/trigger-event :test-key ev 0 1)

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
          (let [ev (sut/->Event 0 1 {:sound :snare})]
            (sut/trigger-event :test-key ev 0 1)

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
          (let [ev (sut/->Event 0 0.25 {:sound :long-slice})]
            (sut/trigger-event :test-key ev 0 1) ; dur-beats = 1
            (let [synth-call (second @mock-calls)
                  args (first (:args synth-call))
                  args-map (apply hash-map args)]
              (is (sut-test/approx= 0.5 (:sustain args-map))))))))))

(deftest slice-info-test
  (testing "sample-info correctly reports slice information"
    (let [buf {:duration 4.0
               :n-channels 2
               :rate 44100.0
               :path "path"
               :size 176400}]
      (with-redefs [samples/samples (atom {:break buf})
                    samples/sample-slices (atom {:kick
                                             {:source :break
                                              :begin 0.0
                                              :end 0.1}})]
        (let [info (sut/sample-info :kick)]
          (is (= :slice (:type info)))
          (is (= :break (:source info)))
          (is (sut-test/approx= 0.4 (:duration info)))
          (is (= 4.0 (:full-duration info)))
          (is (= 0.0 (:begin info)))
          (is (= 0.1 (:end info)))))))

  (testing "sample-info correctly reports slice information with fractions"
    (let [buf {:duration 4.0
               :n-channels 2
               :rate 44100.0
               :path "path"
               :size 176400}]
      (with-redefs [samples/samples (atom {:break buf})
                    samples/sample-slices (atom {:kick
                                             {:source :break
                                              :begin 1/2
                                              :end 3/4}})]
        (let [info (sut/sample-info :kick)]
          (is (= :slice (:type info)))
          (is (= :break (:source info)))
          (is (sut-test/approx= 1.0 (:duration info)))
          (is (= 4.0 (:full-duration info)))
          (is (= 1/2 (:begin info)))
          (is (= 3/4 (:end info)))))))

  (testing "sample-info handles reversed slices"
    (let [buf {:duration 4.0
               :n-channels 2
               :rate 44100.0
               :path "path"
               :size 176400}]
      (with-redefs [samples/samples (atom {:break buf})
                    samples/sample-slices (atom {:kick
                                             {:source :break
                                              :begin 0.1
                                              :end 0.0}})]
        (let [info (sut/sample-info :kick)]
          (is (sut-test/approx= 0.4 (:duration info))))))))

(deftest slice-ms-test
  (testing "slice-sample-ms! converts ms to fractions correctly"
    (let [buf {:duration 2.0 :n-channels 2 :rate 44100.0 :path "path" :size 88200}]
      (with-redefs [samples/samples (atom {:break buf})
                    samples/sample-slices (atom {})]
        ;; 2.0s = 2000ms. Slicing 500ms to 1500ms should be 0.25 to 0.75
        (sut/slice-sample-ms! :ms-slice :break 500 1500)
        (let [slice (get @samples/sample-slices :ms-slice)]
          (is (sut-test/approx= 0.25 (:begin slice)))
          (is (sut-test/approx= 0.75 (:end slice))))))))
