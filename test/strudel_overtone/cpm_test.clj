(ns strudel-overtone.cpm-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest cpm-test
  (testing "cpm updates metro bpm with 4x multiplier"
    (let [bpm-atom (atom 120)]
      (with-redefs [sut/metro (fn [& args]
                                (when (= :bpm (first args))
                                  (reset! bpm-atom (second args))))
                    ov/metro-bpm (fn [_] @bpm-atom)]
        ;; Test setting CPM
        (sut/cpm 30)
        (is (= 120 @bpm-atom))
        (sut/cpm 60)
        (is (= 240 @bpm-atom))
        ;; Test getting CPM
        (is (= 60.0 (double (sut/cpm))))))))

(deftest fade-cpm-test
  (testing "fade-cpm schedules updates"
    (let [bpm-atom (atom 120)
          tasks (atom [])
          current-beat 100
          mock-metro (fn [& args]
                       (cond
                         (empty? args) current-beat
                         (number? (first args)) (* (first args) 1000) ;; map beat to fake ms
                         (= :bpm (first args)) (reset! bpm-atom (second args))))]
      (with-redefs [sut/metro mock-metro
                    ov/metro-bpm (fn [_] @bpm-atom)
                    ov/apply-at (fn [time func]
                                  (swap! tasks conj {:time time :func func}))]

        ;; Start at 30 CPM (120 BPM)
        (sut/cpm 30)

        ;; Fade to 60 CPM over 1 cycle, 8 steps per cycle (8 steps total)
        (sut/fade-cpm 60 1 8)

        (is (= 8 (count @tasks)))

        ;; Check first task
        (let [t1 (first @tasks)]
           ;; beat-offset = 0.5 (step-dur = 4/8 = 0.5)
           ;; time = (metro (+ 100 0.5)) = 100.5 * 1000 = 100500
           (is (= 100500.0 (double (:time t1))))
           ;; Check if function updates cpm
           ((:func t1))
           ;; start-cpm = 30. target = 30 + (1 * step-size). step-size = (60-30)/8 = 3.75
           ;; val = 33.75. BPM = 33.75 * 4 = 135
           (is (= 135.0 (double @bpm-atom))))

        ;; Check last task
        (let [t8 (last @tasks)]
           ;; time = (metro (+ 100 4.0)) = 104000
           (is (= 104000.0 (double (:time t8))))
           ((:func t8))
           ;; target = 60. BPM = 240
           (is (= 240.0 (double @bpm-atom))))))))
