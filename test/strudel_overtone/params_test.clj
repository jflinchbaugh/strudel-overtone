(ns strudel-overtone.params-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
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
                  (sut/mod-index 10))]
      (let [ev (first (:events pat))]
        (is (= 0.5 (get-in ev [:params :pan])))
        (is (= 0.2 (get-in ev [:params :resonance])))
        (is (= 0.05 (get-in ev [:params :attack])))
        (is (= 0.2 (get-in ev [:params :decay])))
        (is (= 0.4 (get-in ev [:params :s-level])))
        (is (= 0.5 (get-in ev [:params :release])))
        (is (= 0.6 (get-in ev [:params :width])))
        (is (= 2 (get-in ev [:params :carrier-ratio])))
        (is (= 3 (get-in ev [:params :modulator-ratio])))
        (is (= 10 (get-in ev [:params :mod-index]))))))

  (testing "trigger-event passes parameters to synth"
     (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)
                    ov/apply-at (fn [ms func args] (swap! mock-calls conj {:func func :args args}))
                    sut/saw-adsr (fn [& args] args)] ;; Mock synth

        (let [pat (-> (sut/note [:c4])
                      (sut/s [:saw])
                      (sut/pan 0.5)
                      (sut/resonance 0.2))
              ev (first (:events pat))]

          (sut/trigger-event ev 0 1)

          ;; Filter for the synth call (ignore log call)
          (let [synth-call (second @mock-calls) ;; first is log, second is synth
                args (:args synth-call)]
            ;; Check if args contains :pan 0.5 and :resonance 0.2
            ;; args is a list/vector of keywords and values
            (let [args-map (apply hash-map args)]
              (is (= 0.5 (:pan args-map)))
              (is (= 0.2 (:resonance args-map)))))))))

  (testing "env parameter is combinable"
    (let [pat (-> (sut/s [:bd :sd])
                  (sut/env [:perc :adsr]))]
      (is (= "perc" (get-in (first (:events pat)) [:params :env])))
      (is (= "adsr" (get-in (second (:events pat)) [:params :env])))))

  (testing "active parameter parses strings in single value"
    (let [pat (-> (sut/s [:bd])
                  (sut/active "0"))]
      (is (= 0.0 (get-in (first (:events pat)) [:params :active]))))))
