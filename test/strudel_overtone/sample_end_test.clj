(ns strudel-overtone.sample-end-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [strudel-overtone.strudel-overtone-test :as sut-test]
            [overtone.core :as ov]))

(deftest sample-end-sets-sustain-test
  (testing "setting end on a sample sets the sustain automatically"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    sut/samples (atom {"test" {:id 1 :duration 2.0}})
                    sut/sampler-adsr (fn [& args] args)
                    sut/sampler-perc (fn [& args] args)]

        ;; Event with sound "test" and end 0.5
        ;; begin defaults to 0, rate defaults to 1
        ;; total-dur = (0.5 - 0) * 2.0 / 1 = 1.0
        ;; default release = 0.3
        ;; sustain should be 1.0 - 0.3 = 0.7
        (let [ev (sut/->Event 0 1 {:sound "test" :end 0.5})]
          (sut/trigger-event ev 0 1)
          
          (let [synth-call (second @mock-calls) ;; first is println
                args (first (:args synth-call))
                args-map (apply hash-map args)]
            (is (sut-test/approx= 0.7 (:sustain args-map)))))

        (reset! mock-calls [])
        ;; Event with begin 0.1, end 0.6, rate 2, release 0.1
        ;; total-dur = (0.6 - 0.1) * 2.0 / 2 = 0.5
        ;; sustain should be 0.5 - 0.1 = 0.4
        (let [ev (sut/->Event 0 1 {:sound "test" :begin 0.1 :end 0.6 :rate 2.0 :release 0.1})]
          (sut/trigger-event ev 0 1)
          
          (let [synth-call (second @mock-calls)
                args (first (:args synth-call))
                args-map (apply hash-map args)]
            (is (sut-test/approx= 0.4 (:sustain args-map)))))

        (reset! mock-calls [])
        ;; Event with env "perc", attack 0.05, end 0.5
        ;; total-dur = (0.5 - 0) * 2.0 / 1 = 1.0
        ;; sustain should be 1.0 - 0.05 = 0.95
        (let [ev (sut/->Event 0 1 {:sound "test" :end 0.5 :env "perc" :attack 0.05})]
          (sut/trigger-event ev 0 1)
          
          (let [synth-call (second @mock-calls)
                args (first (:args synth-call))
                args-map (apply hash-map args)]
            (is (sut-test/approx= 0.95 (:sustain args-map)))))

        (reset! mock-calls [])
        ;; Explicit sustain should take precedence over end
        (let [ev (sut/->Event 0 1 {:sound "test" :end 0.5 :sustain 5.0})]
          (sut/trigger-event ev 0 1)
          
          (let [synth-call (second @mock-calls)
                args (first (:args synth-call))
                args-map (apply hash-map args)]
            (is (= 5.0 (:sustain args-map)))))))))