(ns strudel-overtone.strudel-overtone-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest parse-mini-test
  (testing "Basic space-separated parsing"
    (let [res (sut/parse-mini "bd sd")]
      (is (= 2 (count res)))
      (is (= "bd" (:value (first res))))
      (is (= 0.0 (:start (first res))))
      (is (= 0.5 (:duration (first res))))
      (is (= "sd" (:value (second res))))
      (is (= 0.5 (:start (second res)))))))

(deftest s-function-test
  (testing "s function creates pattern with sound events"
    (let [pat (sut/s "kick snare")]
      (is (map? pat))
      (is (= 2 (count (:events pat))))
      (is (= "kick" (get-in (first (:events pat)) [:params :sound]))))))

(deftest modifiers-test
  (testing "gain modifier updates events"
    (let [pat (-> (sut/s "bd") (sut/gain 0.5))]
      (is (= 0.5 (get-in (first (:events pat)) [:params :amp])))))

  (testing "lpf modifier updates events"
    (let [pat (-> (sut/s "bd") (sut/lpf 1000))]
      (is (= 1000 (get-in (first (:events pat)) [:params :cutoff])))))

  (testing "chaining note and s"
    (let [pat (-> (sut/note "c3") (sut/s "saw"))]
      (is (= "c3" (get-in (first (:events pat)) [:params :note])))
      (is (= "saw" (get-in (first (:events pat)) [:params :sound]))))))

(deftest play!-test
  (testing "play! starts a loop aligned to 4 beats"
    ;; Reset state
    (reset! sut/player-state {:playing? false :patterns {} :loops #{}})

    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (fn
                                ([] 10.5) ;; Mock current time: 10.5 beats
                                ([b] (* b 1000))) ;; Mock beat->ms conversion
                    ov/apply-at (fn [ms func args]
                                  (swap! mock-calls conj {:ms ms :func func :args args}))]

        ;; Call play!
        (sut/play! :test-pat {:events []})

        ;; Verify state updated
        (is (:playing? @sut/player-state))
        (is (= :test-pat (first (:loops @sut/player-state))))
        (is (= {:events []} (get-in @sut/player-state [:patterns :test-pat])))

        ;; Verify apply-at called with correct quantization
        ;; Current time 10.5. Next multiple of 4 is 12.0.
        ;; 10.5 mod 4 = 2.5. 4 - 2.5 = 1.5. 10.5 + 1.5 = 12.0.
        ;; beat->ms(12.0) -> 12000.

        (is (= 1 (count @mock-calls)))
        (let [call (first @mock-calls)]
          (is (= 12000.0 (:ms call)))
          ;; Check if func is the play-loop var.
          (is (= #'strudel-overtone.strudel-overtone/play-loop (:func call)))
          (is (= [:test-pat 12.0] (:args call)))))))

  (testing "play! does not schedule loop if already running"
    (reset! sut/player-state {:playing? true :patterns {} :loops #{:existing}})
    (let [mock-calls (atom [])]
      (with-redefs [sut/metro (constantly 0)
                    ov/apply-at (fn [& _] (swap! mock-calls conj :called))]
        (sut/play! :existing {:events []})
        (is (empty? @mock-calls))))))
