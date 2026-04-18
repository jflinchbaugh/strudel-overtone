(ns strudel-overtone.glide-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest glide-logic-test
  (testing "trigger-event tracks last-freq and provides slide-from"
    (let [player-state (atom {:playing? true :patterns {} :loops #{} :last-freq {}})
          trigger-calls (atom [])]
      (with-redefs [player/player-state player-state
                    ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    overtone.core/apply-at (fn [ms func & _] (func))
                    player/trigger-single-event (fn [key ev params beat dur-beats voice-idx]
                                               (swap! trigger-calls conj params))]

        (testing "first note has slide-from matching its own frequency (no glide)"
          (reset! trigger-calls [])
          (let [pat (-> (sut/note [:c3]) (sut/s :saw) (sut/glide 0.1))
                ev (first (:events pat))
                freq (ov/midi->hz (ov/note :c3))]
            (sut/trigger-event :p1 ev 0 1)
            (let [args (first @trigger-calls)]
              ;; Our mock just captures params. To test the logic, we need to let it run or
              ;; mock at a lower level. Let's mock at-metro instead to see the final args.
              ))))

      (with-redefs [player/player-state player-state
                    overtone.core/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    overtone.core/apply-at (fn [ms func & _] (func))
                    synths/resolve-synth (constantly (fn [& _] nil))
                    player/at-metro (fn [beat synth-var args]
                                                                (swap! trigger-calls conj (apply hash-map args)))
                    player/at-metro-mono (fn [beat key voice-idx synth-var args]
                                                                     (swap! trigger-calls conj (apply hash-map args)))]

        (testing "first note has slide-from matching its own frequency (no glide)"
          (reset! trigger-calls [])
          (let [pat (-> (sut/note [:c3]) (sut/s :saw) (sut/glide 0.1))
                ev (first (:events pat))
                freq (ov/midi->hz (ov/note :c3))]
            (sut/trigger-event :p1 ev 0 1)
            (let [args (first @trigger-calls)]
              (is (= 0.001 (:slide args)))
              (is (= freq (:slide-from args)))
              (is (= freq (get-in @player-state [:last-freq [:p1 0]]))))))

        (testing "second note has slide-from matching first note"
          (reset! trigger-calls [])
          (let [pat (-> (sut/note [:g3]) (sut/s :saw) (sut/glide 0.1))
                ev (first (:events pat))
                prev-freq (ov/midi->hz (ov/note :c3))]
            (sut/trigger-event :p1 ev 4 1)
            (let [args (first @trigger-calls)]
              (is (= 0.2 (:slide args)))
              (is (= prev-freq (:slide-from args)))
              (is (= (ov/midi->hz (ov/note :g3)) (get-in @player-state [:last-freq [:p1 0]]))))))))))

