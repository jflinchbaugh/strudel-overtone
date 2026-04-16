(ns strudel-overtone.voice-idx-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest mono-voice-indexing-test
  (testing "monophonic chords track voices independently"
    (let [player-state (atom {:playing? true :patterns {} :loops #{} :active-synths {} :last-freq {}})
          single-event-calls (atom [])]
      (with-redefs [sut/player-state player-state
                    ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)
                    sut/trigger-single-event
                    (fn [key ev params beat dur-beats voice-idx]
                      (swap! single-event-calls conj voice-idx))]

        (testing "chord triggers multiple indexed single events"
          (let [pat (-> (sut/note (sut/simul [:c3 :e3])) (sut/s :saw) (sut/mono))
                ev (first (:events pat))]
            (sut/trigger-event :p1 ev 0 1)
            (is (= 2 (count @single-event-calls)))
            (is (= #{0 1} (set @single-event-calls)))))

        (testing "stop! gates off all indexed voices"
          (let [gate-calls (atom [])]
            ;; Manually populate active-synths to test stop! logic
            (swap! player-state assoc :active-synths {[:p1 0] {:inst {:id 1}}
                                                     [:p1 1] {:inst {:id 2}}})
            (with-redefs [strudel-overtone.strudel-overtone/gate-off (fn [inst] (swap! gate-calls conj inst))]
              (sut/stop! :p1)
              (is (= 2 (count @gate-calls)))
              (is (empty? (:active-synths @player-state))))))))))
