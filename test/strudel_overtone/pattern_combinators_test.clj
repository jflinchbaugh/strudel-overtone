(ns strudel-overtone.pattern-combinators-test
  (:require [clojure.test :refer [deftest testing is]]
            [strudel-overtone.core :as sut]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.player :as player]
            [strudel-overtone.midi :as midi])
  (:import [strudel_overtone.pattern Pattern Event DecoratedToken]))

(deftest decorated-token-test
  (testing "decorated? identifies DecoratedToken records"
    (let [tok (p/->DecoratedToken :c4 {:gain 0.5})]
      (is (p/decorated? tok))
      (is (not (p/decorated? :c4)))
      (is (not (p/decorated? [:c4])))))

  (testing "parse-mini handles nested DecoratedTokens"
    (let [tok (p/->DecoratedToken [:c4 :e4] {:amp 0.8})
          evs (p/parse-mini tok)]
      (is (= 2 (count evs)))
      (is (every? #(= 0.8 (get-in % [:params :amp])) evs)))))

(deftest combine-patterns-rest-and-overlay-test
  (testing "combine-patterns with overlay propagating rest events"
    (let [base (sut/note [:- :c4])
          ov-pat (p/overlay (sut/gain [0.2 0.8]))
          combined (sut/with-param base :gain ov-pat)
          evs (:events combined)]
      (is (= 2 (count evs)))
      (let [[e1 e2] evs]
        (is (true? (get-in e1 [:params :rest?])))
        (is (= 0 ((get-in e1 [:params :active]) 0 :active)))
        (is (not (true? (get-in e2 [:params :rest?])))))))

  (testing "combine-patterns with non-overlay rest propagation"
    (let [base (sut/note [:c4 :e4])
          combined (sut/note base [:- :g4])
          evs (:events combined)]
      (is (>= (count evs) 2))
      (is (some #(true? (get-in % [:params :rest?])) evs)))))

(deftest with-helper-variations-test
  (testing "with helper with key-value pairs"
    (let [pat (sut/note [:c3 (sut/with :e3 :amp 0.6 :lpf 900) :g3])
          e2 (second (:events pat))]
      (is (= :e3 (get-in e2 [:params :note])))
      (is (= 0.6 ((get-in e2 [:params :amp]) 0 :amp)))
      (is (= 900 ((get-in e2 [:params :lpf]) 0 :lpf))))))

(deftest curried-effects-and-presets-test
  (testing "curried swing, duck, duck-trigger, duck-attack, duck-release"
    (let [pat (-> (sut/s [:bd :sn])
                  ((sut/swing 0.33))
                  ((sut/duck 0.5))
                  ((sut/duck-trigger 0.8))
                  ((sut/duck-attack 0.005))
                  ((sut/duck-release 0.15)))
          ev (first (:events pat))
          params (:params ev)]
      (is (= 0.33 ((:swing params) 0 :swing)))
      (is (= 0.5 ((:duck params) 0 :duck)))
      (is (= 0.8 ((:duck-trigger params) 0 :duck-trigger)))
      (is (= 0.005 ((:duck-attack params) 0 :duck-attack)))
      (is (= 0.15 ((:duck-release params) 0 :duck-release)))))

  (testing "curried sound design effect modifiers"
    (let [pat (-> (sut/note [:c3])
                  ((sut/chaos 1.5))
                  ((sut/coef 0.7))
                  ((sut/crush 0.6))
                  ((sut/distort 0.4))
                  ((sut/hpf 120))
                  ((sut/bpf 440))
                  ((sut/room 0.7))
                  ((sut/room-size 0.8))
                  ((sut/damp 0.3))
                  ((sut/vibrato 5.0))
                  ((sut/echo-delay 0.3))
                  ((sut/echo-repeats 6)))
          ev (first (:events pat))
          params (:params ev)]
      (is (= 1.5 ((:chaos params) 0 :chaos)))
      (is (= 0.7 ((:coef params) 0 :coef)))
      (is (= 0.6 ((:crush params) 0 :crush)))
      (is (= 0.4 ((:distort params) 0 :distort)))
      (is (= 120 ((:hpf params) 0 :hpf)))
      (is (= 440 ((:bpf params) 0 :bpf)))
      (is (= 0.7 ((:room params) 0 :room)))
      (is (= 0.8 ((:room-size params) 0 :room-size)))
      (is (= 0.3 ((:damp params) 0 :damp)))
      (is (= 5.0 ((:vibrato params) 0 :vibrato)))
      (is (= 0.3 ((:delay params) 0 :delay)))
      (is (= 6 ((:repeats params) 0 :repeats))))))

(deftest preset-arities-test
  (testing "acid preset arities"
    (let [p1 (sut/acid (sut/note [:c3]))
          p2 (sut/acid (sut/note [:c3]) 600)
          p3 (sut/acid (sut/note [:c3]) 600 0.1)]
      (is (= 800 ((get-in (first (:events p1)) [:params :lpf]) 0 :lpf)))
      (is (= 600 ((get-in (first (:events p2)) [:params :lpf]) 0 :lpf)))
      (is (= 0.1 ((get-in (first (:events p3)) [:params :resonance])
                  0 :resonance)))))

  (testing "drive preset arities"
    (let [p1 (sut/drive (sut/s [:bd]))
          p2 (sut/drive (sut/s [:bd]) 0.7)]
      (is (= 0.4 ((get-in (first (:events p1)) [:params :distort])
                  0 :distort)))
      (is (= 0.7 ((get-in (first (:events p2)) [:params :distort])
                  0 :distort)))))

  (testing "space preset arities"
    (let [p1 (sut/space (sut/s [:sn]))
          p2 (sut/space (sut/s [:sn]) 0.8)
          p3 (sut/space (sut/s [:sn]) 0.8 0.5)]
      (is (= 0.5 ((get-in (first (:events p1)) [:params :room]) 0 :room)))
      (is (= 0.8 ((get-in (first (:events p2)) [:params :room]) 0 :room)))
      (is (= 0.5 ((get-in (first (:events p3)) [:params :delay])
                  0 :delay))))))

(deftest envelope-helper-generators-test
  (testing "generated env, adsr, perc functions across targets"
    (let [pat (-> (sut/note [:c4])
                  (sut/lpf-env 500)
                  (sut/lpf-perc 0.02)
                  (sut/hpf-perc 0.03)
                  (sut/bpf-perc 0.04)
                  (sut/res-perc 0.05)
                  (sut/phaser-perc 0.06)
                  (sut/crush-perc 0.07)
                  (sut/detune-perc 0.08)
                  (sut/pshift-perc 0.09)
                  (sut/fshift-perc 0.10)
                  (sut/pan-perc 0.11)
                  (sut/distort-perc 0.12))
          params (:params (first (:events pat)))]
      (is (= 500 ((:lpf-env params) 0 :lpf-env)))
      (is (= :perc (:lpf-env-type params)))
      (is (= 0.02 ((:lpf-attack params) 0 :lpf-attack)))
      (is (= 0.12 ((:distort-attack params) 0 :distort-attack))))))

(deftest probabilistic-and-cycle-arities-test
  (testing "sometimes and degrade arity permutations"
    (let [base (sut/s [:bd :sd])]
      ;; 1-arity curried
      (is (fn? (sut/sometimes sut/rev)))
      (let [p1 ((sut/sometimes sut/rev) base)]
        (is (instance? Pattern p1)))
      ;; 2-arity (number, fn) -> curried
      (is (fn? (sut/sometimes 0.8 sut/rev)))
      (let [p2 ((sut/sometimes 0.8 sut/rev) base)]
        (is (instance? Pattern p2)))
      ;; 3-arity permutations
      (is (instance? Pattern (sut/sometimes base 0.5 sut/rev)))
      (is (instance? Pattern (sut/sometimes 0.5 sut/rev base)))
      (is (instance? Pattern (sut/sometimes base sut/rev 0.5)))
      ;; degrade 1-arity pattern vs number
      (is (instance? Pattern (sut/degrade base)))
      (is (instance? Pattern (sut/degrade 0.3 base)))))

  (testing "every-cycle arity and pattern forms"
    (let [base (sut/s [:bd :sd])]
      ;; curried 2-arity
      (is (fn? (sut/every-cycle 4 sut/rev)))
      (let [p1 ((sut/every-cycle 4 sut/rev) base)]
        (is (instance? Pattern p1)))
      ;; 3-arity with pattern
      (is (instance? Pattern (sut/every-cycle base 4 sut/rev)))
      (is (instance? Pattern (sut/every-cycle 4 sut/rev base)))
      ;; 3-arity curried (n offset f)
      (is (fn? (sut/every-cycle 4 1 sut/rev)))
      ;; 4-arity forms
      (is (instance? Pattern (sut/every-cycle base 4 1 sut/rev)))
      (is (instance? Pattern (sut/every-cycle 4 1 sut/rev base)))
      ;; 4-arity dynamic param fn
      (let [alt-fn (sut/every-cycle 4 1 :loud :soft)]
        (is (fn? alt-fn))
        (is (= :loud (binding [p/*current-cycle* 3] (alt-fn 0 :amp))))
        (is (= :soft (binding [p/*current-cycle* 2] (alt-fn 0 :amp))))))))

(deftest pad-light-and-color-edge-cases-test
  (testing "pad-light and pad-color combinations"
    (let [base (sut/note [:c4 :d4])
          p1 (sut/pad-color base :red)
          p2 (sut/pad-light base 36 :cyan)
          p3 (sut/pad-light base [36 37] [:red :blue])
          single-ev (first (:events base))
          ev-lit (sut/pad-light single-ev :magenta)]
      (is (instance? Pattern p1))
      (is (instance? Pattern p2))
      (is (instance? Pattern p3))
      (is (instance? Event ev-lit))
      (is (fn? (get-in ev-lit [:params :pad-light])))
      ;; Test pad-light 1-arity color currying
      (let [curried-color (sut/pad-light :green)]
        (is (fn? curried-color))
        (is (instance? Pattern (curried-color base)))))))
