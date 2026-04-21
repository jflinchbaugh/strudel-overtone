(ns strudel-overtone.delay-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [overtone.core :as ov]))

(deftest delay-cycles-test
  (testing "play! honors delay-cycles on initial start"
    (let [player-state (atom {:playing? false :patterns {} :loops #{}})
          mock-calls (atom [])]
      (with-redefs [player/player-state player-state
                    player/metro (fn
                                   ([] 10.5)
                                   ([b] (* b 1000)))
                    ov/metro-bpm (constantly 120)
                    ov/metronome (fn [& _] player/metro)
                    ov/apply-by (fn [ms func args]
                                  (swap! mock-calls
                                    conj {:ms ms :func func :args args}))]

        (testing "integer delay (2 cycles = 8 beats)"
          (reset! mock-calls [])
          (let [pat (-> (sut/s [:bd]) (sut/delay-cycles 2))]
            (sut/play! :p1 pat)
            ;; now=10.5. next-quant=12.0. delay=8.0. start-beat=20.0
            (let [call (first @mock-calls)]
              (is (= 20000.0 (:ms call)))
              (is (= [:p1 20.0] (:args call))))))

        (testing "fractional delay (0.5 cycles = 2 beats)"
          (reset! mock-calls [])
          (let [pat (-> (sut/s [:bd]) (sut/delay-cycles 0.5))]
            (sut/play! :p2 pat)
            ;; now=10.5. next-quant=12.0. delay=2.0. start-beat=14.0
            (let [call (first @mock-calls)]
              (is (= 14000.0 (:ms call)))
              (is (= [:p2 14.0] (:args call))))))

        (testing "no delay (default 0)"
          (reset! mock-calls [])
          (let [pat (sut/s [:bd])]
            (sut/play! :p3 pat)
            ;; now=10.5. next-quant=12.0. delay=0. start-beat=12.0
            (let [call (first @mock-calls)]
              (is (= 12000.0 (:ms call)))
              (is (= [:p3 12.0] (:args call))))))))))
