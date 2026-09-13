(ns strudel-overtone.player-lifecycle-test
  (:require [clojure.test :refer [deftest testing is]]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]))

(deftest cpm-and-glide-test
  (testing "cpm getter and setter"
    (let [bpm-atom (atom 480)]
      (with-redefs [player/metro (fn [& args]
                                   (if (seq args)
                                     (reset! bpm-atom (second args))
                                     @bpm-atom))
                    ov/metro-bpm (fn [_] @bpm-atom)]
        (is (= 120 (player/cpm)))
        (is (= 140 (player/cpm 140)))
        (is (= 560 @bpm-atom)))))

  (testing "glide-cpm schedules tempo steps"
    (let [scheduled (atom [])
          bpm-atom (atom 480)]
      (with-redefs [player/metro (fn [& args]
                                   (if (seq args)
                                     (reset! bpm-atom (second args))
                                     @bpm-atom))
                    ov/metro-bpm (fn [_] @bpm-atom)
                    ov/apply-at (fn [t f]
                                  (swap! scheduled conj [t f]))]
        (player/glide-cpm 140 2 2)
        (is (= 4 (count @scheduled)))
        ;; Execute scheduled step function
        (let [[_ f] (first @scheduled)]
          (f)
          (is (> (player/cpm) 120)))))))

(deftest at-metro-execution-test
  (testing "at-metro invokes synth-var with args at scheduled beat"
    (let [invoked (atom nil)]
      (with-redefs [player/metro (fn [beat] (* beat 1000))
                    overtone.osc/osc-send-bundle
                    (fn [client bundle]
                      (reset! invoked {:client client :bundle bundle}))]
        (player/at-metro 4 (fn [& args] (vec args)) [:freq 440])
        (is (some? @invoked))))))

(deftest mono-inst-lifecycle-test
  (testing "update-mono-inst calls ctl with partitioned args"
    (let [ctl-calls (atom [])]
      (with-redefs [ov/ctl (fn [inst & args]
                             (swap! ctl-calls conj
                                    {:inst inst :args args}))]
        (player/update-mono-inst 99 [:freq 440 :amp 0.5])
        (is (= [{:inst 99 :args '(:freq 440 :amp 0.5)}] @ctl-calls)))))

  (testing "start-mono-inst starts synth and manages state"
    (let [gate-calls (atom [])
          synth-calls (atom [])
          p-state (atom {:active-synths {}})]
      (with-redefs [player/player-state p-state
                    player/gate-off (fn [inst]
                                      (swap! gate-calls conj inst))]
        (player/start-mono-inst
         :lead 0
         (fn [& args]
           (swap! synth-calls conj (vec args))
           {:id 501})
         [:freq 220 :amp 0.7]
         {:id 500})
        (is (= [{:id 500}] @gate-calls))
        (let [args-map (apply hash-map (first @synth-calls))]
          (is (= 1 (:gate args-map)))
          (is (= 220 (:freq args-map)))
          (is (= 0.7 (:amp args-map))))
        (is (= {:inst {:id 501}}
               (select-keys (get-in @p-state [:active-synths [:lead 0]])
                            [:inst])))))))

(deftest poly-transition-and-gating-test
  (testing "mono voice transition to poly gates off previous mono synth"
    (let [gated (atom [])
          p-state (atom {:active-synths {[:bass 0] {:inst {:id 601}}}})]
      (with-redefs [player/player-state p-state
                    player/metro (constantly 0)
                    ov/metro-bpm (constantly 120)
                    player/gate-off (fn [inst] (swap! gated conj inst))
                    ov/apply-at (fn [_ f] (f))
                    player/at-metro (constantly nil)]
        (player/trigger-single-event
         :bass
         (sut/->Event 0 1 {:sound :saw})
         {:sound :saw :monophonic false}
         0.0 1.0 0)
        (is (= [{:id 601}] @gated))
        (is (nil? (get-in @p-state [:active-synths [:bass 0]]))))))

  (testing "stop! clears duck bus and resets control bus"
    (let [p-state (atom {:playing? true
                         :loops #{:main}
                         :patterns {:main {}}
                         :active-synths {}})
          bus-set (atom nil)]
      (with-redefs [player/player-state p-state
                    player/gate-off (constantly nil)
                    player/metro (constantly 0)
                    ov/server-connected? (constantly true)
                    synths/get-duck-bus (constantly :duck-bus-mock)
                    ov/control-bus-set! (fn [b v] (reset! bus-set [b v]))]
        (sut/stop!)
        (is (= [:duck-bus-mock 0] @bus-set))
        (is (false? (:playing? @p-state)))))))

(deftest play-only-synth-cleanup-test
  (testing "play-only! gates off removed synths for obsolete keys"
    (let [gated (atom [])
          p-state (atom {:playing? true
                         :loops #{:p1 :p2}
                         :patterns {:p1 (sut/s [:bd]) :p2 (sut/s [:hh])}
                         :active-synths {[:p1 0] {:inst {:id 701}}
                                         [:p2 0] {:inst {:id 702}}}})]
      (with-redefs [player/player-state p-state
                    player/metro (constantly 0)
                    player/gate-off (fn [inst] (swap! gated conj inst))
                    ov/apply-by (fn [& _] nil)]
        (sut/play-only! :p2 (sut/s [:hh]))
        (is (= [{:id 701}] @gated))
        (is (nil? (get-in @p-state [:active-synths [:p1 0]])))
        (is (some? (get-in @p-state [:active-synths [:p2 0]])))))))

(deftest chord-in-sequence-trigger-test
  (testing "trigger-event handles chord within a vector of notes"
    (let [calls (atom [])]
      (with-redefs [player/trigger-single-event
                    (fn [k ev p beat dur vidx]
                      (swap! calls conj {:note (:note p)
                                         :vidx vidx
                                         :dur dur}))]
        (let [ev (sut/->Event 0 1 {:note [:c4 #{:e4 :g4}] :sound :saw})]
          (sut/trigger-event :poly ev 0.0 1.0 0 0 1)
          (is (>= (count @calls) 3))
          (let [notes (map :note @calls)]
            (is (some #(= :c4 %) notes))
            (is (some #(= :e4 %) notes))
            (is (some #(= :g4 %) notes))))))))

(deftest player-edge-cases-test
  (testing "glide-cpm with default steps-per-cycle"
    (let [scheduled (atom [])
          bpm-atom (atom 480)]
      (with-redefs [player/metro (fn [& args]
                                   (if (seq args)
                                     (reset! bpm-atom (second args))
                                     @bpm-atom))
                    ov/metro-bpm (fn [_] @bpm-atom)
                    ov/apply-at (fn [t f]
                                  (swap! scheduled conj [t f]))]
        (player/glide-cpm 140 2)
        (is (= 2 (count @scheduled))))))

  (testing "trigger-single-event with string legato and pad-light hook"
    (let [pad-calls (atom [])
          synth-calls (atom [])]
      (with-redefs [player/metro (constantly 0)
                    ov/metro-bpm (constantly 120)
                    synths/resolve-synth (constantly (fn [& args] args))
                    ov/apply-at (fn [ms f & args] (apply f args))
                    player/at-metro (fn [b s-var args]
                                      (swap! synth-calls conj args))]
        (let [ev (sut/->Event 0 1 {:note :c4
                                   :sound :saw
                                   :legato "1.5"
                                   :sustain "0.8"
                                   :pad-light (fn [t k]
                                                (swap! pad-calls conj [t k]))})]
          (player/trigger-single-event :test-pad ev (:params ev) 0 1 0)
          (is (= 1 (count @pad-calls)))
          (is (= 1 (count @synth-calls)))))))

  (testing "play-loop error logging"
    (let [p-state (atom {:playing? true
                         :loops #{:err}
                         :patterns {:err {:cycles (fn [& _]
                                                    (throw
                                                     (Exception. "boom")))}}})
          rescheduled (atom [])]
      (with-redefs [player/player-state p-state
                    player/metro (constantly 0)
                    ov/apply-by (fn [ms f args]
                                  (swap! rescheduled conj [ms f args]))]
        (player/play-loop :err 0 0)
        (is (= 1 (count @rescheduled)))))))
