(ns strudel-overtone.env-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest synth-lookup-test
  (testing "get-synth-name resolves aliases"
    (is (= :kick (#'synths/get-synth-name :bd {})))
    (is (= :snare (#'synths/get-synth-name :snare {}))))

  (testing "resolve-synth finds existing synths"
    (is (var? (#'synths/resolve-synth :kick)))
    (is (var? (#'synths/resolve-synth :saw)))
    (is (nil? (#'synths/resolve-synth :non-existent-synth)))))

(deftest env-param-test
  (testing "env function sets param"
    (let [pat (-> (sut/s [:saw]) (sut/env :perc))]
      (is (= :perc (get-in (first (:events pat)) [:params :env])))))

  (testing "trigger-event passes env-type to synth args"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    player/at-metro (fn [beat synth-var args] (swap! mock-calls conj {:func synth-var :args [args]}))
                    synths/saw (fn [& args] args)]

        (let [pat (-> (sut/note [:c4])
                      (sut/s [:saw])
                      (sut/env :perc))
              ev (first (:events pat))]

          (sut/trigger-event :test-key ev 0 1)
          (let [synth-call (second @mock-calls)
                args (first (:args synth-call))
                args-map (apply hash-map args)]
            (is (= 1 (:env-type args-map)))))))))

(deftest adsr-sig-test
  (testing "adsr-sig evaluates attack, decay, sustain, and release phases"
    (let [sig (sut/adsr-sig 0.1 0.2 0.5 0.2 100 1000)]
      (is (= 100.0 (sig 0.0 nil)))
      (is (= 1000.0 (sig 0.1 nil)))
      (is (= 550.0 (sig 0.3 nil)))
      (is (= 550.0 (sig 0.5 nil)))
      (is (= 100.0 (sig 1.0 nil))))))

(deftest filter-env-param-test
  (testing "lpf-env, lpf-adsr, and lpf-perc set pattern parameters correctly"
    (let [pat (-> (sut/s [:saw])
                  (sut/lpf 300)
                  (sut/lpf-env 5000)
                  (sut/lpf-adsr 0.01 0.15 0.1 0.2))
          ev (first (:events pat))
          params (:params ev)]
      (is (= 5000 ((:lpf-env params) 0 :lpf-env)))
      (is (= :adsr (:lpf-env-type params)))
      (is (= 0.01 ((:lpf-attack params) 0 :lpf-attack)))
      (is (= 0.15 ((:lpf-decay params) 0 :lpf-decay)))
      (is (= 0.1 ((:lpf-s-level params) 0 :lpf-s-level)))
      (is (= 0.2 ((:lpf-release params) 0 :lpf-release)))))

  (testing "hpfe and bpf-adsr set pattern parameters correctly"
    (let [pat (-> (sut/s [:saw])
                  (sut/hpf-env 2000)
                  (sut/hpf-adsr 0.02 0.1 0.5 0.3)
                  (sut/bpf-env 1000)
                  (sut/bpf-perc 0.05))
          ev (first (:events pat))
          params (:params ev)]
      (is (= 2000 ((:hpf-env params) 0 :hpf-env)))
      (is (= :adsr (:hpf-env-type params)))
      (is (= 0.02 ((:hpf-attack params) 0 :hpf-attack)))
      (is (= 1000 ((:bpf-env params) 0 :bpf-env)))
      (is (= :perc (:bpf-env-type params)))
      (is (= 0.05 ((:bpf-attack params) 0 :bpf-attack)))))

  (testing "res-env, phaser-env, and crush-env set pattern parameters correctly"
    (let [pat (-> (sut/s [:saw])
                  (sut/res-env 0.8)
                  (sut/res-adsr 0.01 0.1 0.0 0.1)
                  (sut/phaser-env 0.5)
                  (sut/phaser-perc 0.2)
                  (sut/crush-env 0.9)
                  (sut/crush-adsr 0.001 0.05 0.0 0.1))
          ev (first (:events pat))
          params (:params ev)]
      (is (= 0.8 ((:res-env params) 0 :res-env)))
      (is (= :adsr (:res-env-type params)))
      (is (= 0.5 ((:phaser-env params) 0 :phaser-env)))
      (is (= :perc (:phaser-env-type params)))
      (is (= 0.9 ((:crush-env params) 0 :crush-env)))
      (is (= :adsr (:crush-env-type params))))))
