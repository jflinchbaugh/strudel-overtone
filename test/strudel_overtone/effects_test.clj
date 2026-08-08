(ns strudel-overtone.effects-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [strudel-overtone.samples :as samples]
            [overtone.core :as ov]))

(deftest effect-params-test
  (testing "new effect parameter functions exist"
    (let [pat (-> (sut/s [:bd])
                  (sut/pshift 12)
                  (sut/fshift 100)
                  (sut/tremolo-hz 5)
                  (sut/tremolo-depth 0.5)
                  (sut/pan-hz 1)
                  (sut/pan-depth 0.8)
                  (sut/phaser-hz 0.5)
                  (sut/phaser-depth 0.6))]
      (let [ev (first (:events pat))
            params (:params ev)]
        (is (= 12 ((:pshift params) 0 :pshift)))
        (is (= 100 ((:fshift params) 0 :fshift)))
        (is (= 5 ((:tremolo-hz params) 0 :tremolo-hz)))
        (is (= 0.5 ((:tremolo-depth params) 0 :tremolo-depth)))
        (is (= 1 ((:pan-hz params) 0 :pan-hz)))
        (is (= 0.8 ((:pan-depth params) 0 :pan-depth)))
        (is (= 0.5 ((:phaser-hz params) 0 :phaser-hz)))
        (is (= 0.6 ((:phaser-depth params) 0 :phaser-depth))))))

  (testing "trigger-event passes new effects to sampler"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    player/at-metro (fn [beat synth-var args] (swap! mock-calls conj {:func synth-var :args [args]}))
                    samples/samples (atom {"test" {:id 1 :duration 1}})
                    synths/sampler (fn [& args] args)]

        (let [pat (-> (sut/s [:test])
                      (sut/pshift 7)
                      (sut/fshift -50))
              ev (first (:events pat))]

          (sut/trigger-event :test-key ev 0 1)

          (let [synth-call (second @mock-calls)
                args-map (apply hash-map (first (:args synth-call)))]
            (is (= 7 (:pshift args-map)))
            (is (= -50 (:fshift args-map)))))))))
