(ns strudel-overtone.mono-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest monophonic-logic-test
  (testing "monophonic mode reuses synth instance"
    (let [player-state (atom {:playing? true :patterns {} :loops #{} :active-synths {} :last-freq {}})
          update-calls (atom [])
          start-calls (atom [])
          mock-inst {:id 123}]
      (with-redefs [sut/player-state player-state
                    ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)
                    ov/apply-at (fn [ms func & _] (func))
                    sut/resolve-synth (constantly (fn [& _] mock-inst))
                    ;; Mock the new private helpers
                    strudel-overtone.strudel-overtone/update-mono-inst 
                    (fn [inst args] (swap! update-calls conj {:inst inst :args args}))
                    strudel-overtone.strudel-overtone/start-mono-inst 
                    (fn [key synth-var args old-inst]
                      (swap! start-calls conj {:key key :args args})
                      (swap! player-state assoc-in [:active-synths key] {:inst mock-inst :synth synth-var}))]
        
        (testing "first note triggers start-mono-inst"
          (let [pat (-> (sut/note [:c3]) (sut/s :saw) (sut/mono))
                ev (first (:events pat))]
            (sut/trigger-event :p1 ev 0 1)
            (is (= 1 (count @start-calls)))
            (is (= :p1 (:key (first @start-calls))))))

        (testing "second note triggers update-mono-inst"
          (let [pat (-> (sut/note [:g3]) (sut/s :saw) (sut/mono))
                ev (first (:events pat))]
            (sut/trigger-event :p1 ev 1 1)
            (is (= 1 (count @update-calls)))
            (let [args-map (apply hash-map (:args (first @update-calls)))]
              (is (== (double (ov/midi->hz (ov/note :g3))) (double (:freq args-map)))))))))))
