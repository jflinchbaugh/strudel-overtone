(ns strudel-overtone.strudel-overtone-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(defn approx= [a b]
  (< (Math/abs (- a b)) 0.01))

(deftest play!-test
  (testing "play! quantization logic"
    ;; Reset state
    (reset! sut/player-state {:playing? false :patterns {} :loops #{}})

    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (fn
                                ([] 10.5) ;; Mock current time: 10.5 beats
                                ([b] (* b 1000))) ;; Mock beat->ms conversion
                    ov/metro-bpm (constantly 120)
                    ov/metronome (fn [& _] sut/metro)
                    ov/apply-at (fn [ms func & args]
                                  (swap! mock-calls conj {:ms ms :func func :args args}))
                    ov/apply-by (fn [ms func args]
                                  (swap! mock-calls conj {:ms ms :func func :args args}))
                    ov/at (fn [ms body] (body))]

        (testing "first loop starts on the next cycle (quant = 4)"
          (reset! mock-calls [])
          (sut/play! :p1 {:events []})
          ;; 10.5 -> next multiple of 4 is 12.0
          (let [call (first @mock-calls)]
            (is (= 12000.0 (:ms call)))
            (is (= [:p1 12.0] (:args call)))))

        (testing "subsequent loops align to 4-beat cycle (quant = 4)"
          (reset! mock-calls [])
          ;; :p1 is now in :loops
          (sut/play! :p2 {:events []})
          ;; 10.5 -> next multiple of 4 is 12.0
          (let [call (first @mock-calls)]
            (is (= 12000.0 (:ms call)))
            (is (= [:p2 12.0] (:args call)))))))))

(deftest play!-return-value-test
  (testing "play! returns the names of the patterns"
    (reset! sut/player-state {:playing? false :patterns {} :loops #{}})
    (with-redefs [sut/metro (constantly 0)
                  ov/metro-bpm (constantly 120)
                  ov/metronome (constantly (constantly 0))
                  ov/apply-by (fn [& _] nil)]
      (is (= '(:main) (sut/play! {:events []})))
      (is (= '(:p1 :p2) (sut/play! :p1 {:events []} :p2 {:events []}))))))

(deftest play!-multi-test
  (testing "play! handles multiple patterns"
    (reset! sut/player-state {:playing? false :patterns {} :loops #{}})
    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (fn ([] 10.5) ([b] (* b 1000)))
                    ov/metro-bpm (constantly 120)
                    ov/metronome (fn [& _] sut/metro)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    ov/apply-by (fn [ms func args] (swap! mock-calls conj {:func func :args args}))]

        (sut/play! :p1 {:events []} :p2 {:events []})

        (is (:playing? @sut/player-state))
        (is (contains? (:loops @sut/player-state) :p1))
        (is (contains? (:loops @sut/player-state) :p2))
        (is (= {:events []} (get-in @sut/player-state [:patterns :p1])))
        (is (= {:events []} (get-in @sut/player-state [:patterns :p2])))

        ;; Should have 2 calls to schedule loops
        (is (= 2 (count @mock-calls)))))))

(deftest play!-single-arg-test
  (testing "play! with single argument defaults to :main"
    (reset! sut/player-state {:playing? false :patterns {} :loops #{}})
    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (fn ([] 10.5) ([b] (* b 1000)))
                    ov/metro-bpm (constantly 120)
                    ov/metronome (fn [& _] sut/metro)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    ov/apply-by (fn [ms func args] (swap! mock-calls conj {:func func :args args}))]

        (sut/play! {:events []})

        (is (:playing? @sut/player-state))
        (is (contains? (:loops @sut/player-state) :main))
        (is (= {:events []} (get-in @sut/player-state [:patterns :main])))
        (is (= 1 (count @mock-calls)))))))

(deftest play!-extra-test
  (testing "play! does not schedule loop if already running"
    (reset! sut/player-state {:playing? true :patterns {} :loops #{:existing}})
    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (constantly 0)
                    ov/metro-bpm (constantly 120)
                    ov/metronome (fn [& _] sut/metro)
                    ov/apply-at (fn [& _] (swap! mock-calls conj :called))]
        (sut/play! :existing {:events []})
        (is (empty? @mock-calls))))))

(deftest play!-return-value-test
  (testing "play! returns the names of the patterns"
    (reset! sut/player-state {:playing? false :patterns {} :loops #{}})
    (with-redefs [sut/metro (constantly 0)
                  ov/apply-by (fn [& _] nil)]
      (is (= '(:main) (sut/play! {:events []})))
      (is (= '(:p1 :p2) (sut/play! :p1 {:events []} :p2 {:events []}))))))

(deftest play!-multi-test
  (testing "play! handles multiple patterns"
    (reset! sut/player-state {:playing? false :patterns {} :loops #{}})
    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (fn ([] 10.5) ([b] (* b 1000)))
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:ms ms :func func :args args}))
                    ov/apply-by (fn [ms func args] (swap! mock-calls conj {:ms ms :func func :args args}))]

        (sut/play! :p1 {:events []} :p2 {:events []})

        (is (:playing? @sut/player-state))
        (is (contains? (:loops @sut/player-state) :p1))
        (is (contains? (:loops @sut/player-state) :p2))
        (is (= {:events []} (get-in @sut/player-state [:patterns :p1])))
        (is (= {:events []} (get-in @sut/player-state [:patterns :p2])))

        ;; Should have 2 calls to schedule loops
        (is (= 2 (count @mock-calls)))))))

(deftest play!-single-arg-test
  (testing "play! with single argument defaults to :main"
    (reset! sut/player-state {:playing? false :patterns {} :loops #{}})
    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (fn ([] 10.5) ([b] (* b 1000)))
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:ms ms :func func :args args}))
                    ov/apply-by (fn [ms func args] (swap! mock-calls conj {:ms ms :func func :args args}))]

        (sut/play! {:events []})

        (is (:playing? @sut/player-state))
        (is (contains? (:loops @sut/player-state) :main))
        (is (= {:events []} (get-in @sut/player-state [:patterns :main])))
        (is (= 1 (count @mock-calls)))))))

(deftest play!-extra-test
  (testing "play! does not schedule loop if already running"
    (reset! sut/player-state {:playing? true :patterns {} :loops #{:existing}})
    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (constantly 0)
                    ov/apply-at (fn [& _] (swap! mock-calls conj :called))]
        (sut/play! :existing {:events []})
        (is (empty? @mock-calls)))))

  (testing "sine-synth triggers correctly"
    (reset! sut/player-state {:playing? false :patterns {} :loops #{}})
    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (fn ([] 10.5) ([b] (* b 1000)))
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    ov/apply-by (fn [& _] nil)
                    sut/trigger-event (fn [ev b d] (swap! mock-calls conj {:event ev :sound (get-in ev [:params :sound])}))]
        (sut/play! :sine (-> (sut/note [:c4]) (sut/s [:sine-synth])))
        ;; Simulate the loop running one iteration
        (sut/play-loop :sine 12.0)
        (let [trigger-call (first (filter :event @mock-calls))]
          (is (= "sine-synth" (:sound trigger-call))))))))

(deftest s-function-list-test
  (testing "s function accepts a list of strings"
    (let [pat (sut/s '("bd" "sd"))]
      (is (= 2 (count (:events pat))))
      (is (= "bd" (get-in (first (:events pat)) [:params :sound])))
      (is (= "sd" (get-in (second (:events pat)) [:params :sound])))))

  (testing "s function accepts a vector of strings"
    (let [pat (sut/s ["bd" "sd"])]
      (is (= 2 (count (:events pat))))
      (is (= "bd" (get-in (first (:events pat)) [:params :sound])))))

  (testing "s function accepts a list of symbols"
    (let [pat (sut/s '(bd sd))]
      (is (= 2 (count (:events pat))))
      (is (= "bd" (get-in (first (:events pat)) [:params :sound])))
      (is (= "sd" (get-in (second (:events pat)) [:params :sound])))))

  (testing "s function accepts a vector of keywords"
    (let [pat (sut/s [:bd :sd])]
      (is (= 2 (count (:events pat))))
      (is (= "bd" (get-in (first (:events pat)) [:params :sound])))))

  (testing "s function with underscores in list"
    (let [pat (sut/s [:bd :_ :sd])]
      (is (= 2 (count (:events pat))))
      (is (= "bd" (get-in (first (:events pat)) [:params :sound])))
      (is (= "sd" (get-in (second (:events pat)) [:params :sound])))
      ;; Check timing for the gap: 3 elements, so sd should be at 2/3
      (is (approx= 0.666 (:time (second (:events pat))))))))

(deftest active-test
  (testing "active function marks events as inactive"
    (let [pat (-> (sut/s [:bd :sd]) (sut/active [0 0]))]
      (is (= 0 (get-in (first (:events pat)) [:params :active])))
      (is (= 0 (get-in (second (:events pat)) [:params :active]))))

    (let [pat (-> (sut/s [:bd :sd]) (sut/active [1 0]))]
      (is (= 1 (get-in (first (:events pat)) [:params :active])))
      (is (= 0 (get-in (second (:events pat)) [:params :active])))))

  (testing "trigger-event respects active parameter"
    (let [mock-calls (atom [])]
      (with-redefs [ov/apply-at (fn [& _] (swap! mock-calls conj :log-called))
                    sut/at-metro (fn [beat synth-var args] (swap! mock-calls conj :at-metro-called))
                    sut/resolve-synth (constantly (fn [& _] (swap! mock-calls conj :synth-called)))
                    ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)]
        (testing "active event is triggered"
          (reset! mock-calls [])
          (sut/trigger-event (sut/->Event 0 1 {:sound "bd" :active 1}) 0 1)
          (is (= 2 (count @mock-calls)))) ;; log-called and at-metro-called

        (testing "inactive event is not triggered"
          (reset! mock-calls [])
          (sut/trigger-event (sut/->Event 0 1 {:sound "bd" :active 0}) 0 1)
          (is (empty? @mock-calls)))

        (testing "inactive event with boolean false is not triggered"
          (reset! mock-calls [])
          (sut/trigger-event (sut/->Event 0 1 {:sound "bd" :active false}) 0 1)
          (is (empty? @mock-calls)))

        (testing "event without active param is triggered"
          (reset! mock-calls [])
          (sut/trigger-event (sut/->Event 0 1 {:sound "bd"}) 0 1)
          (is (= 2 (count @mock-calls)))))))) ;; log-called and at-metro-called
