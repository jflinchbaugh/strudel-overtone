(ns strudel-overtone.glide-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest glide-logic-test
  (testing "trigger-event tracks last-freq and provides slide-from"
    (let [player-state (atom {:playing? true :patterns {} :loops #{} :last-freq {}})
          trigger-calls (atom [])]
      (with-redefs [sut/player-state player-state
                    ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)
                    ov/apply-at (fn [& _] nil)
                    sut/at-metro (fn [beat synth-var args]
                                   (swap! trigger-calls conj (apply hash-map args)))
                    sut/resolve-synth (constantly (fn [& _] nil))]
        
        (testing "first note has slide-from matching its own frequency (no glide)"
          (reset! trigger-calls [])
          (let [pat (-> (sut/note [:c3]) (sut/s :saw) (sut/glide 0.1))
                ev (first (:events pat))
                freq (ov/midi->hz (ov/note :c3))]
            (sut/trigger-event :p1 ev 0 1)
            (let [args (first @trigger-calls)]
              (is (= 0.001 (:slide args))) ;; No last-freq, so slide is minimal
              (is (= freq (:slide-from args))) ;; slide-from is itself
              (is (= freq (get-in @player-state [:last-freq :p1]))))))

        (testing "second note has slide-from matching first note"
          (reset! trigger-calls [])
          (let [pat (-> (sut/note [:g3]) (sut/s :saw) (sut/glide 0.1))
                ev (first (:events pat))
                prev-freq (ov/midi->hz (ov/note :c3))]
            (sut/trigger-event :p1 ev 4 1)
            (let [args (first @trigger-calls)]
              (is (= 0.2 (:slide args)))
              (is (= prev-freq (:slide-from args)))
              (is (= (ov/midi->hz (ov/note :g3)) (get-in @player-state [:last-freq :p1]))))))))))
