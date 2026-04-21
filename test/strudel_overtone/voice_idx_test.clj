(ns strudel-overtone.voice-idx-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest mono-voice-indexing-test
  (testing "monophonic chords track voices independently"
    (let [player-state (atom {:playing? true :patterns {} :loops #{} :last-freq {}})
          single-event-calls (atom [])]
      (with-redefs [player/player-state player-state
                    ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    player/trigger-single-event (fn [key ev params beat dur-beats voice-idx]
                                                  (swap! single-event-calls conj voice-idx))]
        
        (testing "chord triggers multiple indexed single events"
          ;; We create an event manually that contains a chord in its params
          ;; to trigger the splitting logic in trigger-event.
          (let [ev (sut/->Event 0 1 {:note #{:c3 :e3} :sound :saw :monophonic 1})]
            (sut/trigger-event :p1 ev 0 1)
            (is (= 2 (count @single-event-calls)))
            (is (= #{0 1} (set @single-event-calls)))))

        (testing "stop! defers gating for all indexed voices"
          (let [gate-calls (atom [])]
            ;; Manually populate active-synths to test stop! logic
            (swap! player-state assoc :loops #{:p1}
                           :active-synths {[:p1 0] {:inst {:id 1}}
                                          [:p1 1] {:inst {:id 2}}})
            (with-redefs [player/gate-off (fn [inst] (swap! gate-calls conj inst))
                          player/metro (constantly 0)
                          ov/apply-by (fn [& _] nil)]
              (sut/stop! :p1)
              ;; Should NOT be gated yet
              (is (= 0 (count @gate-calls)))
              
              ;; Simulate play-loop running its final cleanup iteration
              (player/play-loop :p1 0)
              
              (is (= 2 (count @gate-calls)))
              (is (empty? (:active-synths @player-state))))))))))
