(ns strudel-overtone.effects-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
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
        (is (= 12 (:pshift params)))
        (is (= 100 (:fshift params)))
        (is (= 5 (:tremolo-hz params)))
        (is (= 0.5 (:tremolo-depth params)))
        (is (= 1 (:pan-hz params)))
        (is (= 0.8 (:pan-depth params)))
        (is (= 0.5 (:phaser-hz params)))
        (is (= 0.6 (:phaser-depth params))))))

  (testing "trigger-event passes new effects to sampler"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    sut/samples (atom {"test" {:id 1 :duration 1}})
                    sut/sampler-adsr (fn [& args] args)]

        (let [pat (-> (sut/s [:test])
                      (sut/pshift 7)
                      (sut/fshift -50))
              ev (first (:events pat))]

          (sut/trigger-event ev 0 1)

          (let [synth-call (second @mock-calls)
                args-map (apply hash-map (first (:args synth-call)))]
            (is (= 7 (:pshift args-map)))
            (is (= -50 (:fshift args-map)))))))))
