(ns strudel-overtone.random-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(defn approx= [a b]
  (< (abs (- a b)) 0.01))

(deftest repeatable-rand-test
  (testing "repeatable-rand is deterministic"
    (sut/seed! 123)
    (let [r1 (#'sut/repeatable-rand 1.0 :amp)
          r2 (#'sut/repeatable-rand 1.0 :amp)
          r3 (#'sut/repeatable-rand 1.1 :amp)
          r4 (#'sut/repeatable-rand 1.0 :pan)]
      (is (= r1 r2))
      (is (not= r1 r3))
      (is (not= r1 r4))))

  (testing "seed! changes the result"
    (sut/seed! 123)
    (let [r1 (#'sut/repeatable-rand 1.0 :amp)]
      (sut/seed! 456)
      (let [r2 (#'sut/repeatable-rand 1.0 :amp)]
        (is (not= r1 r2))))))

(deftest srand-stream-test
  (testing "srand stream returns different values for different times"
    (sut/seed! 0)
    (let [rs (sut/srand 0 10)]
      (is (not= (rs 0.0 :amp) (rs 1.0 :amp)))
      (is (>= (rs 0.0 :amp) 0))
      (is (< (rs 0.0 :amp) 10))))

  (testing "irand stream returns integers"
    (sut/seed! 0)
    (let [irs (sut/irand 10)]
      (is (integer? (irs 0.0 :amp)))
      (is (>= (irs 0.0 :amp) 0))
      (is (< (irs 0.0 :amp) 10)))))

(deftest choose-stream-test
  (testing "choose stream picks from collection"
    (sut/seed! 0)
    (let [cs (sut/choose [:a :b :c])]
      (is (contains? #{:a :b :c} (cs 0.0 :amp)))
      (is (contains? #{:a :b :c} (cs 1.0 :amp))))))

(deftest wchoose-stream-test
  (testing "wchoose stream picks based on weights"
    (sut/seed! 0)
    ;; High weight for :a, low for :b
    (let [ws (sut/wchoose [[:a 100] [:b 0]])]
      (is (= :a (ws 0.0 :amp)))
      (is (= :a (ws 1.0 :amp))))

    ;; Zero weight for :a, high for :b
    (let [ws (sut/wchoose [[:a 0] [:b 100]])]
      (is (= :b (ws 0.0 :amp))))))

(deftest choose-n-test
  (testing "choose-n returns a sequence of n functions"
    (let [cf (sut/choose-n 4 [:c2 :e2 :g2])]
      (is (= 4 (count cf)))
      (is (every? fn? cf))
      (let [notes (map (fn [f] (f 0.0 :note)) cf)]
        (is (every? #(contains? #{:c2 :e2 :g2} %) notes))))))

(deftest rtake-test
  (testing "rtake from a random sequence"
    (is (= 3 (count (sut/rtake 3 :this (sut/irand 10))))
      "take 3 values from the stream")
    (is (= (sut/rtake 5 :this (sut/irand 10))
          (sut/rtake 5 :this (sut/irand 10)))
      "reproducible")
    (is (not= (sut/rtake 5 :this (sut/irand 10))
           (sut/rtake 5 :that (sut/irand 10)))
      "different ids get different results")
    ))

(deftest signal-test
  (testing "sine signal"
    (is (approx= 0.5 (sut/sine 0.0 nil)))
    (is (approx= 1.0 (sut/sine 0.25 nil)))
    (is (approx= 0.5 (sut/sine 0.5 nil)))
    (is (approx= 0.0 (sut/sine 0.75 nil))))

  (testing "saw signal"
    (is (= 0.0 (sut/saw 0.0 nil)))
    (is (= 0.5 (sut/saw 0.5 nil)))
    (is (= 0.0 (sut/saw 1.0 nil))))

  (testing "tri signal"
    (is (= 0.0 (sut/tri 0.0 nil)))
    (is (= 1.0 (sut/tri 0.5 nil)))
    (is (= 0.0 (sut/tri 1.0 nil))))

  (testing "sig-range scaling"
    (let [s (sut/sig-range sut/sine 100 200)]
      (is (approx= 150 (s 0.0 nil)))
      (is (approx= 200 (s 0.25 nil)))
      (is (approx= 100 (s 0.75 nil))))))

(deftest trigger-event-with-streams-test
  (testing "trigger-event resolves stream functions for numeric params"
    (let [mock-calls (atom [])]
      (with-redefs [ov/apply-at (fn [& _] (swap! mock-calls conj :log-called))
                    sut/at-metro (fn [beat synth-var args]
                                   (swap! mock-calls conj {:beat beat :args (apply hash-map args)}))
                    sut/resolve-synth (constantly (fn [& _] nil))
                    ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)]
        (sut/seed! 0)
        (let [ev (sut/->Event 0 1 {:sound "saw" :amp (sut/srand 0.5 0.6)})]
          (sut/trigger-event ev 10.0 1)
          (let [args (:args (second @mock-calls))
                amp (:amp args)]
            (is (>= amp 0.5))
            (is (<= amp 0.6)))))))

  (testing "trigger-event resolves stream functions for sound param"
    (let [mock-calls (atom [])]
      (with-redefs [ov/apply-at (fn [& _] (swap! mock-calls conj :log-called))
                    sut/at-metro (fn [beat synth-var args]
                                   (swap! mock-calls conj {:beat beat :synth synth-var}))
                    sut/resolve-synth (fn [s] (when (= s "kick") (fn [& _] nil)))
                    ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)]
        (sut/seed! 0)
        ;; choose that returns "kick"
        (let [ev (sut/->Event 0 1 {:sound (sut/choose ["kick" "kick"])})]
          (sut/trigger-event ev 10.0 1)
          (let [synth (:synth (second @mock-calls))]
            (is (fn? synth))))))))
