(ns strudel-overtone.legato-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [overtone.core :as ov]))

(deftest legato-param-test
  (testing "legato scales the step duration"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [& _] nil)
                    player/at-metro (fn [beat synth-var args] 
                                      (swap! mock-calls conj (apply hash-map args)))]
        
        (testing "legato 1.0 (default)"
          (reset! mock-calls [])
          (let [pat (-> (sut/s :saw) (sut/legato 1.0))
                ev (first (:events pat))]
            (sut/trigger-event :p1 ev 0 1) ;; 1 beat = 0.5s at 120 BPM
            (is (= 0.5 (:sustain (first @mock-calls))))))

        (testing "legato 2.0 (overlap)"
          (reset! mock-calls [])
          (let [pat (-> (sut/s :saw) (sut/legato 2.0))
                ev (first (:events pat))]
            (sut/trigger-event :p1 ev 0 1)
            (is (= 1.0 (:sustain (first @mock-calls))))))

        (testing "legato 0.5 (staccato)"
          (reset! mock-calls [])
          (let [pat (-> (sut/s :saw) (sut/legato 0.5))
                ev (first (:events pat))]
            (sut/trigger-event :p1 ev 0 1)
            (is (= 0.25 (:sustain (first @mock-calls))))))))))
