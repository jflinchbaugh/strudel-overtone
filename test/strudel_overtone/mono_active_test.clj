(ns strudel-overtone.mono-active-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.player :as player]
            [strudel-overtone.pattern :as p]
            [overtone.core :as ov]))

(deftest mono-deactivation-test
  (testing "deactivating a mono synth should gate it off"
    (let [gate-calls (atom [])
          mock-inst {:id 999}
          player-state (atom {:playing? true
                             :loops #{:p1}
                             :active-synths {[:p1 0] {:inst mock-inst :synth :saw}}})]
      (with-redefs [player/player-state player-state
                    player/gate-off (fn [inst] (swap! gate-calls conj inst))
                    player/metro (constantly 0)
                    ov/apply-at (fn [_ f] (f))]
        
        ;; Trigger an INACTIVE event for the same key/voice
        (let [ev (p/->Event 0 1 {:sound :saw :monophonic 1 :active (constantly 0)})]
          (player/trigger-event :p1 ev 0 1 0)
          
          (is (= 1 (count @gate-calls)) "Should have gated off the synth")
          (is (= 999 (:id (first @gate-calls))))
          (is (empty? (:active-synths @player-state)) "Should have removed from active-synths"))))))
