(ns strudel-overtone.synths-test
  (:require [clojure.test :refer [deftest testing is]]
            [strudel-overtone.core :as sut]
            [strudel-overtone.synths :as synths]
            [strudel-overtone.additive-showcase :as additive]
            [overtone.core :as ov]))

(deftest synth-helpers-test
  (testing "get-synth-name maps aliases correctly"
    (is (= :kick (synths/get-synth-name :bd nil)))
    (is (= :snare (synths/get-synth-name :sd nil)))
    (is (= :hat (synths/get-synth-name :hh nil)))
    (is (= :clap (synths/get-synth-name :cp nil)))
    (is (= :saw (synths/get-synth-name :saw nil)))
    (is (= :mooger (synths/get-synth-name :mooger nil))))

  (testing "supports-mono? identifies poly vs mono synths"
    (is (false? (synths/supports-mono? :kick)))
    (is (false? (synths/supports-mono? :bd)))
    (is (false? (synths/supports-mono? :hat)))
    (is (false? (synths/supports-mono? :sampler)))
    (is (false? (synths/supports-mono? :white)))
    (is (true? (synths/supports-mono? :saw)))
    (is (true? (synths/supports-mono? :sine)))
    (is (true? (synths/supports-mono? :mooger)))
    (is (true? (synths/supports-mono? :tb303))))

  (testing "resolve-synth resolves vars in strudel-overtone.synths"
    (is (some? (synths/resolve-synth :saw)))
    (is (some? (synths/resolve-synth :tb303)))
    (is (some? (synths/resolve-synth :supersaw)))
    (is (some? (synths/resolve-synth :mooger)))
    (is (some? (synths/resolve-synth :crackle)))
    (is (nil? (synths/resolve-synth :non-existent-synth)))))

(deftest get-duck-bus-test
  (testing "get-duck-bus returns control bus and reuses existing bus"
    (let [bus-mock :mock-duck-bus]
      (with-redefs [synths/duck-bus-atom (atom nil)
                    ov/server-connected? (constantly true)
                    ov/control-bus (fn [_] bus-mock)]
        (let [b1 (synths/get-duck-bus)
              b2 (synths/get-duck-bus)]
          (is (= bus-mock b1))
          (is (= bus-mock b2)))))))

(deftest def-additive-macro-test
  (testing "def-additive! macro expands and registers additive synth var"
    (let [v (synths/resolve-synth :add-organ)]
      (is (some? v))
      (is (ifn? @v)))))
