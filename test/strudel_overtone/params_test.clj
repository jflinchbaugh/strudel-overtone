(ns strudel-overtone.params-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest param-functions-test
  (testing "parameter functions exist and work on patterns"
    (let [pat (-> (sut/note [:c4])
                  (sut/s [:saw])
                  (sut/pan 0.5)
                  (sut/resonance 0.2)
                  (sut/adsr 0.05 0.2 0.4 0.5)
                  (sut/width 0.6)
                  (sut/carrier-ratio 2)
                  (sut/modulator-ratio 3)
                  (sut/mod-index 10)
                  (sut/echo-repeats 8))]
      (let [ev (first (:events pat))
            params (:params ev)]
        (is (= 0.5 ((:pan params) 0 :pan)))
        (is (= 0.2 ((:resonance params) 0 :resonance)))
        (is (= :adsr (get-in ev [:params :env])))
        (is (= 0.05 ((:attack params) 0 :attack)))
        (is (= 0.2 ((:decay params) 0 :decay)))
        (is (= 0.4 ((:s-level params) 0 :s-level)))
        (is (= 0.5 ((:release params) 0 :release)))
        (is (= 0.6 ((:width params) 0 :width)))
        (is (= 2 ((:carrier-ratio params) 0 :carrier-ratio)))
        (is (= 3 ((:modulator-ratio params) 0 :modulator-ratio)))
        (is (= 10 ((:mod-index params) 0 :mod-index)))
        (is (= 8 ((:repeats params) 0 :repeats)))))))

(deftest atom-param-test
  (testing "atoms passed as parameters automatically deref"
    (let [cutoff-atom (atom 800)
          pat (-> (sut/s [:saw])
                  (sut/lpf cutoff-atom))
          ev (first (:events pat))
          lpf-fn (get-in ev [:params :lpf])]
      (is (fn? lpf-fn))
      (is (= 800 (lpf-fn 0 :lpf)))
      (reset! cutoff-atom 1500)
      (is (= 1500 (lpf-fn 0 :lpf))))))


(deftest echo-params-test
  (testing "echo and echo-delay set correct parameters"
    (let [pat (-> (sut/s [:bd])
                  (sut/echo-delay 0.25)
                  (sut/echo-repeats 10))
          ev (first (:events pat))
          params (:params ev)]
      (is (= 0.25 ((:delay params) 0 :delay)))
      (is (= 10 ((:repeats params) 0 :repeats))))

    (let [pat (-> (sut/s [:bd])
                  (sut/echo 0.3 5))
          ev (first (:events pat))
          params (:params ev)]
      (is (= 0.3 ((:delay params) 0 :delay)))
      (is (= 5 ((:repeats params) 0 :repeats))))

    (let [pat (-> (sut/s [:bd])
                  (sut/echo))
          ev (first (:events pat))
          params (:params ev)]
      (is (= 0.25 ((:delay params) 0 :delay)))
      (is (= 4 ((:repeats params) 0 :repeats))))))

(deftest trigger-event-params-test
  (testing "trigger-event passes parameters to synth"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    player/at-metro (fn [beat synth-var args] (swap! mock-calls conj {:func synth-var :args [args]}))
                    synths/saw (fn [& args] args)] ;; Mock synth

        (let [pat (-> (sut/note [:c4])
                      (sut/s [:saw])
                      (sut/pan 0.5)
                      (sut/resonance 0.2)
                      (sut/detune 10)
                      (sut/vibrato 5))
              ev (first (:events pat))]

          (sut/trigger-event :test-key ev 0 1)

          ;; Filter for the synth call (ignore log call)
          (let [synth-call (second @mock-calls) ;; first is log, second is synth
                args (:args synth-call)]
            ;; Check if args contains :pan 0.5 and :resonance 0.2
            ;; args is a list/vector of keywords and values
            (let [args-map (apply hash-map (first args))]
              (is (= 0.5 (:pan args-map)))
              (is (= 0.2 (:resonance args-map)))
              (is (= 10 (:detune args-map)))
              (is (= 5 (:vibrato args-map))))))))))

(deftest note-rests-test
  (testing "note handles :- and :_ as rests"
    (let [pat (sut/note [:c3 :- :e3 :_])
          events (:events pat)]
      (is (= 4 (count events)))
      (let [e2 (nth events 1)
            e4 (nth events 3)
            active2 ((get-in e2 [:params :active]) 0 :active)
            active4 ((get-in e4 [:params :active]) 0 :active)]
        (is (= 0 active2) "middle rest :- should be inactive")
        (is (= 0 active4) "end rest :_ should be inactive")))))

(deftest trigger-event-rest-test
  (testing "trigger-event handles rests without MIDI parsing error"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (func))
                    player/at-metro (fn [beat synth-var args] (swap! mock-calls conj {:func synth-var :args [args]}))
                    synths/saw (fn [& args] args)]

        (let [pat (-> (sut/note [:-]) (sut/s [:saw]))
              ev (first (:events pat))]
          ;; This should not throw an IllegalArgumentException
          (is (nil? (sut/trigger-event :test-rest ev 0 1))))))))

(deftest with-glide-expansion-test
  (testing "with-glide macro expansion includes detune and vibrato"
    (let [expansion (macroexpand '(strudel-overtone.synths/with-glide 440 (ov/saw actual-f)))]
      (is (clojure.string/includes? (str expansion) "detune"))
      (is (clojure.string/includes? (str expansion) "vibrato")))))

(deftest env-param-test
  (testing "env parameter is combinable"
    (let [pat (-> (sut/s [:bd :sd])
                  (sut/env [:perc :adsr]))]
      (is (= :perc (get-in (first (:events pat)) [:params :env])))
      (is (= :adsr (get-in (second (:events pat)) [:params :env]))))))

(deftest parameter-splitting-test
  (testing "parameter patterns split events"
    (let [pat (-> (sut/s [:kick]) (sut/gain [0 1]))
          events (:events pat)]
      (is (= 2 (count events)))
      (let [ev1 (nth events 0)
            ev2 (nth events 1)]
        (is (= 0.0 (:time ev1)))
        (is (= 0.5 (:duration ev1)))
        (is (== 0 ((get-in ev1 [:params :amp]) 0 :amp)))

        (is (= 0.5 (:time ev2)))
        (is (= 0.5 (:duration ev2)))
        (is (== 1 ((get-in ev2 [:params :amp]) 0.5 :amp)))))))

(deftest overlay-test
  (testing "overlay should apply parameters without splitting base events"
    (let [pat (-> (sut/s [:kick])
                  (sut/gain (sut/overlay [0.1 0.2])))
          events (:events pat)]
      (is (= 1 (count events)) "Base event should not be split")
      (let [ev (first events)
            amp-fn (get-in ev [:params :amp])]
        (is (= 0.0 (:time ev)))
        (is (= 1.0 (:duration ev)))
        (is (== 0.1 (amp-fn 0 :amp)) "Should sample first value at start time")))))

(deftest additive-params-test
  (testing "gain/amp, add, detune, pshift, fshift, vibrato, and envelope depths combine additively when chained"
    (let [pat (-> (sut/note [:c3])
                  (sut/gain 0.4)
                  (sut/gain 0.3)
                  (sut/add 12)
                  (sut/add 4)
                  (sut/detune 5)
                  (sut/detune 10)
                  (sut/pshift 7)
                  (sut/pshift 5)
                  (sut/fshift 100)
                  (sut/fshift 50)
                  (sut/vibrato 0.5)
                  (sut/vibrato 0.2)
                  (sut/lpf-env 2000)
                  (sut/lpf-env 3000))
          ev (first (:events pat))
          params (:params ev)]
      (is (= 0.7 ((:amp params) 0 :amp)))
      (is (= 16.0 ((:add params) 0 :add)))
      (is (= 15.0 ((:detune params) 0 :detune)))
      (is (= 12.0 ((:pshift params) 0 :pshift)))
      (is (= 150.0 ((:fshift params) 0 :fshift)))
      (is (= 0.7 ((:vibrato params) 0 :vibrato)))
      (is (= 5000.0 ((:lpf-env params) 0 :lpf-env))))))

(deftest light-grid-param-test
  (testing "light-grid pattern modifier with static, direct, and factory fns"
    (let [grid-calls (atom [])]
      (with-redefs [strudel-overtone.midi/light-grid! (fn [f]
                                                        (swap! grid-calls conj (f 0 1)))]
        ;; 1. Direct 2-arg (r, c) color fn
        (let [pat (-> (sut/s [:kick])
                      (sut/light-grid (fn [r c] :red)))
              ev (first (:events pat))
              hook (get-in ev [:params :light-grid])]
          (is (fn? hook))
          (hook 0 :light-grid)
          (is (= [:red] @grid-calls)))

        ;; 2. Factory fn (time) -> (r, c)
        (reset! grid-calls [])
        (let [pat (-> (sut/s [:kick])
                      (sut/light-grid (fn [t]
                                        (fn [r c] (if (zero? t) :blue :yellow)))))
              ev (first (:events pat))
              hook (get-in ev [:params :light-grid])]
          (hook 0.0 :light-grid)
          (is (= [:blue] @grid-calls))
          (hook 1.5 :light-grid)
          (is (= [:blue :yellow] @grid-calls)))

        ;; 3. Factory fn (cycle, time) -> (r, c)
        (reset! grid-calls [])
        (let [pat (-> (sut/s [:kick])
                      (sut/light-grid (fn [cycle t]
                                        (fn [r c] [cycle t r c]))))
              ev (first (:events pat))
              hook (get-in ev [:params :light-grid])]
          (binding [strudel-overtone.pattern/*current-cycle* 3]
            (hook 3.25 :light-grid)
            (is (= [[3 3.25 0 1]] @grid-calls))))))))

(deftest pad-light-test
  (testing "pad-light as constructor and pattern modifier"
    (let [pad-events (atom [])]
      (with-redefs [strudel-overtone.midi/light-on! (fn [pad color]
                                                      (swap! pad-events conj [pad color]))]
        ;; 1. Direct constructor: (pad-light [[0 0] [0 1]] :green)
        (let [pat (sut/pad-light [[0 0] [0 1]] :green)
              evs (:events pat)]
          (is (= 2 (count evs)))
          (let [hook1 (get-in (first evs) [:params :pad-light])]
            (is (fn? hook1))
            (hook1 0.0 :pad-light)
            (is (= [[0 :green]] @pad-events)))
          (let [hook2 (get-in (second evs) [:params :pad-light])]
            (hook2 0.5 :pad-light)
            (is (= [[0 :green] [1 :green]] @pad-events))))

        ;; 2. Modifying an existing note pattern with a color: (-> (note [:c3]) (pad-light :cyan))
        (reset! pad-events [])
        (let [pat (-> (sut/note [:c3])
                      (sut/pad-light :cyan))
              ev (first (:events pat))
              hook (get-in ev [:params :pad-light])]
          (hook 0.0 :pad-light)
          (is (= [[:c3 :cyan]] @pad-events)))

        ;; 3. Modifying an existing sound pattern with pad coordinates and color
        (reset! pad-events [])
        (let [pat (-> (sut/s [:bd :sd])
                      (sut/pad-light [[0 0] [0 1]] :yellow))
              evs (:events pat)]
          (is (= 2 (count evs)))
          ((get-in (first evs) [:params :pad-light]) 0.0 :pad-light)
          ((get-in (second evs) [:params :pad-light]) 0.5 :pad-light)
          (is (= [[0 :yellow] [1 :yellow]] @pad-events)))))))
