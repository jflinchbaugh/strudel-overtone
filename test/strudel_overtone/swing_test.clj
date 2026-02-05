(ns strudel-overtone.swing-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

;; Access private function
(def apply-swing @#'sut/apply-swing)

(deftest apply-swing-test
  (testing "apply-swing delays odd steps"
    ;; Grid = 8 (0.125 step size)
    ;; Even steps: 0 (0.0), 2 (0.25), 4 (0.5), 6 (0.75) -> No Change
    (is (= 0.0 (apply-swing 0.0 0.5 0.125)))
    (is (= 0.25 (apply-swing 0.25 0.5 0.125)))

    ;; Odd steps: 1 (0.125), 3 (0.375), etc -> Delayed
    ;; Delay = amount * step_size
    ;; swing 0.5, step 0.125 -> delay 0.0625
    ;; 0.125 + 0.0625 = 0.1875
    (is (= 0.1875 (apply-swing 0.125 0.5 0.125)))
    
    ;; swing 0.1, step 0.125 -> delay 0.0125
    ;; 0.375 + 0.0125 = 0.3875
    (is (= 0.3875 (apply-swing 0.375 0.1 0.125)))))

(deftest dynamic-grid-test
  (testing "grid is inferred from min duration"
    ;; Pattern: [:bd :sd] -> Durs: 0.5, 0.5. Min: 0.5 (Quarter notes)
    ;; Swing should apply to 0.5 steps.
    ;; 0.0 -> Even. 0.5 -> Odd (swung).
    
    ;; Since we can't easily inspect play-loop internal state, we rely on the
    ;; fact that play-loop uses min-duration as the 3rd arg to apply-swing.
    ;; We verified the logic in apply-swing-test.
    
    ;; Let's verify the calculation logic manually for what play-loop does:
    (let [evs [{:time 0.0 :duration 0.5} {:time 0.5 :duration 0.5}]
          min-dur (apply min (map :duration evs))]
      (is (= 0.5 min-dur))
      (is (= 0.0 (apply-swing 0.0 0.5 min-dur)))
      ;; 0.5 is index 1 of step 0.5. Odd -> Swung.
      (is (= 0.75 (apply-swing 0.5 0.5 min-dur)))) ;; 0.5 + (0.5 * 0.5) = 0.75
      
    ;; Pattern: [:bd [:sd :hh]] -> Durs: 0.5, 0.25, 0.25. Min: 0.25 (8th notes)
    (let [evs [{:time 0.0 :duration 0.5} 
               {:time 0.5 :duration 0.25} 
               {:time 0.75 :duration 0.25}]
          min-dur (apply min (map :duration evs))]
      (is (= 0.25 min-dur))
      ;; 0.0 (idx 0) -> Even
      (is (= 0.0 (apply-swing 0.0 0.5 min-dur)))
      ;; 0.5 (idx 2 of 0.25) -> Even
      (is (= 0.5 (apply-swing 0.5 0.5 min-dur)))
      ;; 0.75 (idx 3 of 0.25) -> Odd -> Swung
      ;; 0.75 + (0.5 * 0.25) = 0.75 + 0.125 = 0.875
      (is (= 0.875 (apply-swing 0.75 0.5 min-dur))))))

(deftest logging-effective-time-test
  (testing "logged event contains effective-time"
    (let [tasks (atom [])
          mock-metro (constantly 1000)
          ;; Mock apply-at to capture the event passed to logging/synth
          mock-apply-at (fn [time func & args]
                          (when (and args (map? (first args))) ;; Capture the event map
                             (swap! tasks conj (first args))))
          
          ;; Define a pattern with swing
          pat (-> (sut/s [:bd :sd]) ;; 0.0, 0.5. Grid 0.5
                  (sut/swing 0.5))
          
          player-state (atom {:playing? true 
                              :patterns {:test pat} 
                              :loops #{:test}})]

      (with-redefs [sut/metro mock-metro
                    sut/player-state player-state
                    ov/apply-at mock-apply-at
                    sut/at-metro (fn [& _] nil) ;; Ignore synth calls for this test
                    ov/metro-bpm (constantly 120) ;; Mock metro-bpm
                    ;; Stub trigger-event dependencies if needed, or just let it run
                    ;; We need to ensure trigger-event calls apply-at with the event
                    sut/resolve-synth (constantly (fn [& _]))]
        
        ;; Run one loop iteration
        (sut/play-loop :test 0)
        
        ;; Expect 2 events.
        ;; Event 1: Start 0.0. Even index (0). No swing. Effective = 0.0
        ;; Event 2: Start 0.5. Odd index (1). Swung. Effective = 0.75
        (is (= 2 (count @tasks)))
        
        (let [ev1 (first @tasks)
              ev2 (second @tasks)]
          
          (is (= 0.0 (:time ev1)))
          (is (= 0.0 (:effective-time ev1)))
          
          (is (= 0.5 (:time ev2)))
          (is (= 0.75 (:effective-time ev2))))))))

(deftest swing-param-test
  (testing "swing param is set on pattern"
    (let [pat (sut/s [:bd :sd])
          swung (sut/swing pat 0.2)
          events (:events swung)]
      (is (= 0.2 (get-in (first events) [:params :swing])))
      (is (= 0.2 (get-in (second events) [:params :swing]))))))
