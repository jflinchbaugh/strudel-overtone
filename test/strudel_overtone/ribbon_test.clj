(ns strudel-overtone.ribbon-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest ribbon-basic-test
  (testing "ribbon should loop a segment of a pattern"
    (let [pat (-> (sut/s [:bd :sd :hh :cp])
                  (sut/ribbon 0 0.5)) ;; first half: :bd and :sd
          evs (:events pat)]
      ;; ribbon generates 100 cycles. Each cycle has 2 events.
      (is (= 200 (count evs)))
      (is (= :bd (get-in (first evs) [:params :sound])))
      (is (= :sd (get-in (second evs) [:params :sound])))
      ;; check that it repeats
      (is (= :bd (get-in (nth evs 2) [:params :sound])))
      (is (= :sd (get-in (nth evs 3) [:params :sound]))))))

(deftest ribbon-freezes-randomness-test
  (testing "ribbon should freeze random values present before the ribbon"
    (let [pat (-> (sut/note (sut/choose-n 4 [:c4 :e4 :g4 :b4]))
                  (sut/s [:saw])
                  (sut/ribbon 0 1))
          player-state (atom {:playing? true :patterns {:test pat} :loops #{:test}})
          results (atom [])]

      (with-redefs [sut/player-state player-state
                    sut/metro (fn ([] 0) ([b] (* b 1000)))
                    ov/apply-by (fn [& _] nil)
                    sut/trigger-event (fn [ev beat dur]
                                        (let [note (get-in ev [:params :note])
                                              resolved-note (if (fn? note) (note beat :note) note)]
                                          (swap! results conj resolved-note)))]

        ;; Play first cycle (beat 0)
        (sut/play-loop :test 0)
        (let [cycle1 @results]
          (is (= 4 (count cycle1)))

          (reset! results [])
          ;; Play second cycle (beat 4)
          (sut/play-loop :test 4)
          (let [cycle2 @results]
            ;; Should be identical to cycle 1 even though absolute beat changed
            (is (= cycle1 cycle2) "Random notes should be frozen by ribbon"))))))

  (testing "parameters added AFTER ribbon should still be random"
    (let [pat (-> (sut/note [:c4])
                  (sut/s [:saw])
                  (sut/ribbon 0 1)
                  (sut/pan (sut/srand -1 1)))
          player-state (atom {:playing? true :patterns {:test pat} :loops #{:test}})
          results (atom [])]

      (with-redefs [sut/player-state player-state
                    sut/metro (fn ([] 0) ([b] (* b 1000)))
                    ov/apply-by (fn [& _] nil)
                    sut/trigger-event (fn [ev beat dur]
                                        (let [pan (get-in ev [:params :pan])
                                              resolved-pan (if (fn? pan) (pan beat :pan) pan)]
                                          (swap! results conj resolved-pan)))]

        (sut/play-loop :test 0)
        (let [pan1 (first @results)]
          (reset! results [])
          (sut/play-loop :test 4)
          (let [pan2 (first @results)]
            (is (not= pan1 pan2) "Pan added after ribbon should still be random")))))))

(deftest ribbon-scrub-test
  (testing "ribbon should allow scrubbing to an offset in a random stream"
    (let [pat (sut/note (sut/choose [:c4 :e4 :g4 :b4]))
          rib0 (sut/ribbon pat 0 1)
          rib10 (sut/ribbon pat 10 1)]

      (sut/seed! 42)
      (let [ev0 (first (:events rib0))
            ev10 (first (:events rib10))
            n0 (when ev0 ((get-in ev0 [:params :note]) 0 :note))
            n10 (when ev10 ((get-in ev10 [:params :note]) 0 :note))]

        (is (some? ev10) "Ribbon should unroll source to reach offset 10")
        (is (not= n0 n10) "Ribbon at different offsets should capture different values")

        ;; check stability of captured value
        (is (= n10 ((get-in ev10 [:params :note]) 0 :note)))
        (is (= n10 ((get-in ev10 [:params :note]) 100.0 :note)))))))

(deftest ribbon-performance-test
  (testing "play-loop should only schedule events for the current cycle"
    (let [pat (-> (sut/s [:bd]) (sut/ribbon 0 0.25))
          player-state (atom {:playing? true
                             :patterns {:test pat}
                             :loops #{:test}})
          trigger-calls (atom [])]
      (with-redefs [sut/player-state player-state
                    sut/metro (fn ([] 0) ([b] (* b 1000)))
                    ov/apply-by (fn [& _] nil)
                    sut/trigger-event (fn [ev beat dur]
                                        (swap! trigger-calls conj beat))]

        (sut/play-loop :test 0)
        ;; ribbon 0.25 on 1 cycle pattern = 4 events per cycle
        (is (= 4 (count @trigger-calls)) 
            "Should only trigger events for the current cycle [0, 1)")))))
