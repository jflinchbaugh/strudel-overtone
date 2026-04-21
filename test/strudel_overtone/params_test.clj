(ns strudel-overtone.params-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest param-functions-test
  (testing "new parameter functions exist and work on patterns"
    (let [pat (-> (sut/note [:c4])
                  (sut/s [:saw])
                  (sut/pan 0.5)
                  (sut/resonance 0.2)
                  (sut/attack 0.05)
                  (sut/decay 0.2)
                  (sut/s-level 0.4)
                  (sut/release 0.5)
                  (sut/width 0.6)
                  (sut/carrier-ratio 2)
                  (sut/modulator-ratio 3)
                  (sut/mod-index 10)
                  (sut/echo-repeats 8))]
      (let [ev (first (:events pat))
            params (:params ev)]
        (is (= 0.5 ((:pan params) 0 :pan)))
        (is (= 0.2 ((:resonance params) 0 :resonance)))
        (is (= 0.05 ((:attack params) 0 :attack)))
        (is (= 0.2 ((:decay params) 0 :decay)))
        (is (= 0.4 ((:s-level params) 0 :s-level)))
        (is (= 0.5 ((:release params) 0 :release)))
        (is (= 0.6 ((:width params) 0 :width)))
        (is (= 2 ((:carrier-ratio params) 0 :carrier-ratio)))
        (is (= 3 ((:modulator-ratio params) 0 :modulator-ratio)))
        (is (= 10 ((:mod-index params) 0 :mod-index)))
        (is (= 8 ((:repeats params) 0 :repeats)))))))

(deftest trigger-event-params-test
  (testing "trigger-event passes parameters to synth"
     (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    player/at-metro (fn [beat synth-var args] (swap! mock-calls conj {:func synth-var :args [args]}))
                    synths/saw-adsr (fn [& args] args)] ;; Mock synth

        (let [pat (-> (sut/note [:c4])
                      (sut/s [:saw])
                      (sut/pan 0.5)
                      (sut/resonance 0.2))
              ev (first (:events pat))]

          (sut/trigger-event :test-key ev 0 1)

          ;; Filter for the synth call (ignore log call)
          (let [synth-call (second @mock-calls) ;; first is log, second is synth
                args (:args synth-call)]
            ;; Check if args contains :pan 0.5 and :resonance 0.2
            ;; args is a list/vector of keywords and values
            (let [args-map (apply hash-map (first args))]
              (is (= 0.5 (:pan args-map)))
              (is (= 0.2 (:resonance args-map))))))))))

(deftest env-param-test
  (testing "env parameter is combinable"
    (let [pat (-> (sut/s [:bd :sd])
                  (sut/env [:perc :adsr]))]
      (is (= :perc (get-in (first (:events pat)) [:params :env])))
      (is (= :adsr (get-in (second (:events pat)) [:params :env]))))))

(deftest parameter-splitting-test
  (testing "parameter patterns split events"
    (let [pat (-> (sut/s [:kick]) (sut/gain [0 1]))
          events (:events pat)]
      (is (= 2 (count events)))
      (let [ev1 (nth events 0)
            ev2 (nth events 1)]
        (is (= 0.0 (:time ev1)))
        (is (= 0.5 (:duration ev1)))
        (is (== 0 ((get-in ev1 [:params :amp]) 0 :amp)))

        (is (= 0.5 (:time ev2)))
        (is (= 0.5 (:duration ev2)))
        (is (== 1 ((get-in ev2 [:params :amp]) 0.5 :amp)))))))

(deftest overlay-test
  (testing "overlay should apply parameters without splitting base events"
    (let [pat (-> (sut/s [:kick])
                  (sut/gain (sut/overlay [0.1 0.2])))
          events (:events pat)]
      (is (= 1 (count events)) "Base event should not be split")
      (let [ev (first events)
            amp-fn (get-in ev [:params :amp])]
        (is (= 0.0 (:time ev)))
        (is (= 1.0 (:duration ev)))
        (is (== 0.1 (amp-fn 0 :amp)) "Should sample first value at start time")))))
