(ns strudel-overtone.duck-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest duck-params-test
  (testing "duck and duck-trigger params are set"
    (let [pat (-> (sut/s [:saw])
                  (sut/duck 0.5)
                  (sut/duck-trigger 0.8))]
      (let [ev (first (:events pat))]
        (is (= 0.5 (get-in ev [:params :duck])))
        (is (= 0.8 (get-in ev [:params :duck-trigger]))))))

  (testing "trigger-event passes duck params to synth"
     (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    sut/at-metro (fn [beat synth-var args] (swap! mock-calls conj {:func synth-var :args [args]}))
                    sut/saw-adsr (fn [& args] args)] ;; Mock synth

        (let [pat (-> (sut/note [:c4])
                      (sut/s [:saw])
                      (sut/duck 0.5)
                      (sut/duck-trigger 1.0))
              ev (first (:events pat))]

          (sut/trigger-event ev 0 1)

          (let [synth-call (second @mock-calls)
                args (:args synth-call)
                args-map (apply hash-map (first args))]
            (is (= 0.5 (:duck args-map)))
            (is (= 1.0 (:duck-trigger args-map)))))))))
