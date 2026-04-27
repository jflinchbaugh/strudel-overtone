(ns strudel-overtone.mono-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))
(deftest monophonic-logic-test
  (testing "monophonic mode reuses synth instance"
    (let [player-state (atom {:playing? true :patterns {} :loops #{} :active-synths {} :last-freq {}})
          mono-calls (atom [])
          mock-inst {:id 123}]
      (with-redefs [player/player-state player-state
                    overtone.core/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    overtone.core/apply-at (fn [ms func & _] (func))
                    synths/resolve-synth (constantly (fn [& _] mock-inst))
                    player/at-metro-mono
                    (fn [beat key voice-idx synth-var args]
                      (swap! mono-calls conj {:key key :voice voice-idx :args args})
                      ;; Manually update player-state to simulate start-mono-inst
                      (swap! player-state assoc-in [:active-synths [key voice-idx]] {:inst mock-inst :synth synth-var}))]

        (testing "first note triggers at-metro-mono"
          (let [pat (-> (sut/note [:c3]) (sut/s :saw) (sut/mono))
                ev (first (:events pat))]
            (sut/trigger-event :p1 ev 0 1)
            (is (= 1 (count @mono-calls)))
            (is (= :p1 (:key (first @mono-calls))))
            (is (= 0 (:voice (first @mono-calls))))))

        (testing "second note triggers at-metro-mono again"
          (let [pat (-> (sut/note [:g3]) (sut/s :saw) (sut/mono))
                ev (first (:events pat))]
            (sut/trigger-event :p1 ev 1 1)
            (is (= 2 (count @mono-calls)))
            (let [args-map (apply hash-map (:args (second @mono-calls)))]
              (is (== (double (ov/midi->hz (ov/note :g3))) (double (:freq args-map)))))))

        (testing "rest triggers gate-off"
          (let [gate-offs (atom 0)]
            (with-redefs [player/gate-off (fn [inst] (swap! gate-offs inc))]
              (let [pat (-> (sut/note [:c3 :-]) (sut/mono))
                    rest-ev (second (:events pat))]
                (is (= 0 (get-in rest-ev [:params :active])))
                (sut/trigger-event :p1 rest-ev 2 1)
                (is (= 1 @gate-offs))
                (is (nil? (get-in @player-state [:active-synths [:p1 0]])))))))))))

