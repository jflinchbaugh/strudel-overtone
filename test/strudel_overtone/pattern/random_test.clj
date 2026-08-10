(ns strudel-overtone.pattern.random-test
  (:require [clojure.test :refer [deftest testing is]]
            [strudel-overtone.pattern.random :as r]))

(deftest repeatable-rand-test
  (testing "repeatable-rand is deterministic"
    (r/seed! 123)
    (let [r1 (r/repeatable-rand 1.0 :amp)
          r2 (r/repeatable-rand 1.0 :amp)
          r3 (r/repeatable-rand 1.1 :amp)
          r4 (r/repeatable-rand 1.0 :pan)]
      (is (= r1 r2))
      (is (not= r1 r3))
      (is (not= r1 r4))))

  (testing "seed! changes the result"
    (r/seed! 123)
    (let [r1 (r/repeatable-rand 1.0 :amp)]
      (r/seed! 456)
      (let [r2 (r/repeatable-rand 1.0 :amp)]
        (is (not= r1 r2))))))

(deftest srand-stream-test
  (testing "srand stream returns different values for different times"
    (r/seed! 0)
    (let [rs (r/srand 0 10)]
      (is (not= (rs 0.0 :amp) (rs 1.0 :amp)))
      (is (>= (rs 0.0 :amp) 0))
      (is (< (rs 0.0 :amp) 10))))

  (testing "irand stream returns integers"
    (r/seed! 0)
    (let [irs (r/irand 10)]
      (is (integer? (irs 0.0 :amp)))
      (is (>= (irs 0.0 :amp) 0))
      (is (< (irs 0.0 :amp) 10)))))

(deftest choose-stream-test
  (testing "choose stream picks from collection"
    (r/seed! 0)
    (let [cs (r/choose [:a :b :c])]
      (is (contains? #{:a :b :c} (cs 0.0 :amp)))
      (is (contains? #{:a :b :c} (cs 1.0 :amp))))))

(deftest wchoose-stream-test
  (testing "wchoose stream picks based on weights"
    (r/seed! 0)
    ;; High weight for :a, low for :b
    (let [ws (r/wchoose [[:a 100] [:b 0]])]
      (is (= :a (ws 0.0 :amp)))
      (is (= :a (ws 1.0 :amp))))

    ;; Zero weight for :a, high for :b
    (let [ws (r/wchoose [[:a 0] [:b 100]])]
      (is (= :b (ws 0.0 :amp))))))

(deftest choose-n-test
  (testing "choose-n returns a sequence of n functions"
    (let [cf (r/choose-n 4 [:c2 :e2 :g2])]
      (is (= 4 (count cf)))
      (is (every? fn? cf))
      (let [notes (map (fn [f] (f 0.0 :note)) cf)]
        (is (every? #(contains? #{:c2 :e2 :g2} %) notes))))))

(deftest rtake-test
  (testing "rtake from a random sequence"
    (is (= 3 (count (r/rtake 3 :this (r/irand 10))))
        "take 3 values from the stream")
    (is (= (r/rtake 5 :this (r/irand 10))
           (r/rtake 5 :this (r/irand 10)))
        "reproducible")
    (is (not= (r/rtake 5 :this (r/irand 10))
              (r/rtake 5 :that (r/irand 10)))
        "different ids get different results")))
