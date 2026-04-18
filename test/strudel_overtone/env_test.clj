(ns strudel-overtone.env-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest synth-lookup-test
  (testing "get-synth-name resolves aliases and defaults"
    (is (= :saw-adsr (#'synths/get-synth-name :saw {})))
    ;; bd aliased to kick, kick is percussive so default is perc
    (is (= :kick-perc (#'synths/get-synth-name :bd {})))
    (is (= :snare-perc (#'synths/get-synth-name :snare {}))))

  (testing "get-synth-name handles env param"
    (is (= :kick-perc (#'synths/get-synth-name :bd {:env :perc})))
    (is (= :saw-perc (#'synths/get-synth-name :saw {:env :perc})))
    (is (= :sine-perc (#'synths/get-synth-name :sine {:env :perc}))))

  (testing "resolve-synth finds existing synths"
    (is (var? (#'synths/resolve-synth :kick-adsr)))
    (is (var? (#'synths/resolve-synth :saw-adsr)))
    (is (var? (#'synths/resolve-synth :saw-perc)))
    (is (nil? (#'synths/resolve-synth :non-existent-synth)))))

(deftest env-param-test
  (testing "env function sets param"
    (let [pat (-> (sut/s [:saw-synth]) (sut/env :perc))]
      (is (= :perc (get-in (first (:events pat)) [:params :env])))))

  (testing "trigger-event calls perc synth"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    player/at-metro (fn [beat synth-var args] (swap! mock-calls conj {:func synth-var :args [args]}))
                    synths/saw-perc (fn [& args] args)] ;; Mock synth var

        (let [pat (-> (sut/note [:c4])
                      (sut/s [:saw-synth])
                      (sut/env :perc))
              ev (first (:events pat))]

          (with-redefs [synths/resolve-synth (fn [name]
                                            (if (= name :saw-synth-perc)
                                              #'synths/saw-perc
                                              nil))]
             (sut/trigger-event :test-key ev 0 1)
             (let [synth-call (second @mock-calls)]
               (is (= #'synths/saw-perc (:func synth-call))))))))))

