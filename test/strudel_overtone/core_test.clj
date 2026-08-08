(ns strudel-overtone.core-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [strudel-overtone.samples :as samples]
            [overtone.core :as ov]))

(defn approx= [a b]
  (< (abs (- a b)) 0.01))

(defn active-val [ev]
  (let [v (get-in ev [:params :active] (constantly 1))]
    (if (fn? v) (v 0 :active) v)))

(use-fixtures :each
  (fn [f]
    (sut/stop!)
    (f)
    (sut/stop!)))

(deftest play!-test
  (testing "play! quantization logic"
    (let [player-state (atom {:playing? false :patterns {} :loops #{}})
          mock-calls (atom [])]
      (with-redefs [player/player-state player-state
                    player/metro (fn
                                   ([] 10.5)
                                   ([b] (* b 1000)))
                    ov/metro-bpm (constantly 120)
                    ov/metronome (fn [& _] player/metro)
                    ov/apply-at (fn [ms func & args]
                                  (swap! mock-calls
                                    conj {:ms ms :func func :args args}))
                    ov/apply-by (fn [ms func args]
                                  (swap! mock-calls
                                    conj {:ms ms :func func :args args}))]

        (testing "first loop starts on the next cycle (quant = 4)"
          (reset! mock-calls [])
          (sut/play! :p1 {:events []})
          ;; 10.5 -> next multiple of 4 is 12.0
          (let [call (first @mock-calls)]
            (is (= 12000.0 (:ms call)))
            (is (= [:p1 12.0 12.0] (:args call)))))

        (testing "subsequent loops align to 4-beat cycle (quant = 4)"
          (reset! mock-calls [])
          ;; :p1 is now in :loops
          (sut/play! :p2 {:events []})
          ;; 10.5 -> next multiple of 4 is 12.0
          (let [call (first @mock-calls)]
            (is (= 12000.0 (:ms call)))
            (is (= [:p2 12.0 12.0] (:args call))))))))

(deftest play!-return-value-test
  (testing "play! returns the names of the patterns"
    (let [player-state (atom {:playing? false :patterns {} :loops #{}})]
      (with-redefs [player/player-state player-state
                    player/metro (constantly 0)
                    ov/metro-bpm (constantly 120)
                    ov/apply-by (fn [& _] nil)]
        (is (= '(:main) (sut/play! {:events []})))
        (is (= '(:p1 :p2) (sut/play! :p1 {:events []} :p2 {:events []})))))))

(deftest play!-multi-test
  (testing "play! handles multiple patterns"
    (let [player-state (atom {:playing? false :patterns {} :loops #{}})
          mock-calls (atom [])]
      (with-redefs [player/player-state player-state
                    player/metro (fn ([] 10.5) ([b] (* b 1000)))
                    ov/metro-bpm (constantly 120)
                    ov/apply-at (fn [ms func & args]
                                  (swap! mock-calls
                                    conj {:func func :args args}))
                    ov/apply-by (fn [ms func args]
                                  (swap! mock-calls
                                    conj {:func func :args args}))]

        (sut/play! :p1 {:events []} :p2 {:events []})

        (is (:playing? @player-state))
        (is (contains? (:loops @player-state) :p1))
        (is (contains? (:loops @player-state) :p2))
        (is (= {:events []} (get-in @player-state [:patterns :p1])))
        (is (= {:events []} (get-in @player-state [:patterns :p2])))

        ;; Should have 2 calls to schedule loops
        (is (= 2 (count @mock-calls)))))))

(deftest play!-single-arg-test
  (testing "play! with single argument defaults to :main"
    (let [player-state (atom {:playing? false :patterns {} :loops #{}})
          mock-calls (atom [])]
      (with-redefs [player/player-state player-state
                    player/metro (fn ([] 10.5) ([b] (* b 1000)))
                    ov/metro-bpm (constantly 120)
                    ov/apply-at (fn [ms func & args]
                                  (swap! mock-calls
                                    conj {:func func :args args}))
                    ov/apply-by (fn [ms func args]
                                  (swap! mock-calls
                                    conj {:func func :args args}))]

        (sut/play! {:events []})

        (is (:playing? @player-state))
        (is (contains? (:loops @player-state) :main))
        (is (= {:events []} (get-in @player-state [:patterns :main])))
        (is (= 1 (count @mock-calls)))))))

(deftest play!-extra-test
  (testing "play! does not schedule loop if already running"
    (let [player-state (atom {:playing? true :patterns {} :loops #{:existing}})
          mock-calls (atom [])]
      (with-redefs [player/player-state player-state
                    player/metro (constantly 0)
                    ov/metro-bpm (constantly 120)
                    ov/apply-at (fn [& _] (swap! mock-calls conj :called))
                    ov/apply-by (fn [& _] (swap! mock-calls conj :called))]
        (sut/play! :existing {:events []})
        (is (empty? @mock-calls)))))

  (testing "sine-synth triggers correctly"
    (let [player-state (atom {:playing? true :patterns {} :loops #{}})
          mock-calls (atom [])]
      (with-redefs [player/player-state player-state
                    player/metro (fn ([] 10.5) ([b] (* b 1000)))
                    ov/metro-bpm (constantly 120)
                    ov/apply-at (fn [ms func & args]
                                  (swap! mock-calls
                                    conj {:func func :args args}))
                    ov/apply-by (fn [& _] nil)
                    player/trigger-event (fn [key ev b d & _]
                                           (swap! mock-calls
                                             conj {:event ev
                                                   :sound (get-in ev [:params
                                                                      :sound])}))]
        (sut/play! :sine (-> (sut/note [:c4]) (sut/s [:sine-synth])))
        ;; Simulate the loop running one iteration
        (player/play-loop :sine 12.0 12.0)
        (let [trigger-call (first (filter :event @mock-calls))]
          (is (= :sine-synth (:sound trigger-call))))))))

(deftest s-function-list-test
  (testing "s function accepts a list of strings"
    (let [pat (sut/s '("bd" "sd"))]
      (is (= 2 (count (:events pat))))
      (is (= :bd (get-in (first (:events pat)) [:params :sound])))
      (is (= :sd (get-in (second (:events pat)) [:params :sound])))))

  (testing "s function accepts a vector of strings"
    (let [pat (sut/s ["bd" "sd"])]
      (is (= 2 (count (:events pat))))
      (is (= :bd (get-in (first (:events pat)) [:params :sound])))))

  (testing "s function accepts a list of symbols"
    (let [pat (sut/s '(bd sd))]
      (is (= 2 (count (:events pat))))
      (is (= :bd (get-in (first (:events pat)) [:params :sound])))
      (is (= :sd (get-in (second (:events pat)) [:params :sound])))))

  (testing "s function accepts a vector of keywords"
    (let [pat (sut/s [:bd :sd])]
      (is (= 2 (count (:events pat))))
      (is (= :bd (get-in (first (:events pat)) [:params :sound])))))

  (testing "s function with underscores in list"
    (let [pat (sut/s [:bd :_ :sd])]
      (is (= 3 (count (:events pat))))
      (is (= :bd (get-in (first (:events pat)) [:params :sound])))
      (is (= 0 (active-val (second (:events pat)))))
      (is (= :sd (get-in (nth (:events pat) 2) [:params :sound])))
      ;; Check timing for the gap: 3 elements, so sd should be at 2/3
      (is (approx= 0.666 (:time (nth (:events pat) 2))))))))

(deftest active-test
  (testing "active function marks events as inactive"
    (let [pat (-> (sut/s [:bd :sd]) (sut/active [0 0]))]
      (is (= 0 (active-val (first (:events pat)))))
      (is (= 0 (active-val (second (:events pat))))))

    (let [pat (-> (sut/s [:bd :sd]) (sut/active [1 0]))]
      (is (= 1 (active-val (first (:events pat)))))
      (is (= 0 (active-val (second (:events pat))))))))

(deftest trigger-event-respects-active-test
  (testing "trigger-event respects active parameter"
    (let [mock-calls (atom [])]
      (with-redefs [ov/apply-at (fn [& _] (swap! mock-calls
                                            conj :log-called))
                    player/at-metro (fn [beat synth-var args]
                                      (swap! mock-calls
                                        conj :at-metro-called))
                    synths/resolve-synth (constantly
                                           (fn [& _]
                                             (swap! mock-calls
                                               conj :synth-called)))
                    ov/metro-bpm (constantly 120)
                    player/metro (constantly 0)]
        (testing "active event is triggered"
          (reset! mock-calls [])
          (sut/trigger-event :test-key (sut/->Event 0 1 {:sound :bd :active (constantly 1)}) 0 1 0)
          (is (= 2 (count @mock-calls)))) ;; log-called and at-metro-called

        (testing "inactive event is not triggered"
          (reset! mock-calls [])
          (sut/trigger-event :test-key (sut/->Event 0 1 {:sound :bd :active (constantly 0)}) 0 1 0)
          (is (empty? @mock-calls)))

        (testing "inactive event with boolean false is not triggered"
          (reset! mock-calls [])
          (sut/trigger-event :test-key (sut/->Event 0 1 {:sound :bd :active (constantly false)}) 0 1 0)
          (is (empty? @mock-calls)))

        (testing "event without active param is triggered"
          (reset! mock-calls [])
          (sut/trigger-event :test-key (sut/->Event 0 1 {:sound :bd}) 0 1 0)
          (is (= 2 (count @mock-calls))))))))

(deftest fast-slow-test
  (testing "fast function multiplies cycles"
    (let [pat {:cycles (constantly 1)}
          fast-pat (sut/fast pat 2)
          cycles-fn (:cycles fast-pat)]
      (is (== 2 (cycles-fn 0 :cycles)))))

  (testing "slow function divides cycles"
    (let [pat {:cycles (constantly 1)}
          slow-pat (sut/slow pat 2)
          cycles-fn (:cycles slow-pat)]
      (is (== 0.5 (cycles-fn 0 :cycles))))))

(deftest def-additive-test
  (testing "def-additive! macro defines expected synth"
    (sut/def-additive! :test-organ [1.0 0.5])
    (is (some? (ns-resolve 'strudel-overtone.synths 'test-organ))))
  (testing "def-additive! with custom step"
    (sut/def-additive! :test-step [1.0 0.5] :step 2)
    (is (some? (ns-resolve 'strudel-overtone.synths 'test-step))))
