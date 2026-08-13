(ns strudel-overtone.pattern.signals-test
  (:require [clojure.test :refer [deftest testing is]]
            [strudel-overtone.pattern.signals :as sig]))

(defn approx= [a b]
  (< (abs (- a b)) 0.01))

(deftest signal-test
  (testing "sine-sig signal"
    (is (approx= 0.5 (sig/sine-sig 0.0 nil)))
    (is (approx= 1.0 (sig/sine-sig 0.25 nil)))
    (is (approx= 0.5 (sig/sine-sig 0.5 nil)))
    (is (approx= 0.0 (sig/sine-sig 0.75 nil))))

  (testing "saw-sig signal"
    (is (= 0.0 (sig/saw-sig 0.0 nil)))
    (is (= 0.5 (sig/saw-sig 0.5 nil)))
    (is (= 0.0 (sig/saw-sig 1.0 nil))))

  (testing "tri-sig signal"
    (is (= 0.0 (sig/tri-sig 0.0 nil)))
    (is (= 1.0 (sig/tri-sig 0.5 nil)))
    (is (= 0.0 (sig/tri-sig 1.0 nil))))

  (testing "sig-range scaling"
    (let [s (sig/sig-range sig/sine-sig 100 200)]
      (is (approx= 150 (s 0.0 nil)))
      (is (approx= 200 (s 0.25 nil)))
      (is (approx= 100 (s 0.75 nil))))))

(sig/def-sig pulse-sig
  "Pulse signal generator."
  [t _]
  (if (< (mod t 1) 0.25) 1 0))

(deftest def-sig-test
  (testing "def-sig multi-arity generation"
    (is (= 1 (pulse-sig 0.1 nil)))
    (is (= 0 (pulse-sig 0.5 nil)))
    (let [f (pulse-sig 2)]
      (is (= 1 (f 0.05 nil)))
      (is (= 0 (f 0.25 nil))))
    (let [scaled (pulse-sig 1 10 20)]
      (is (= 20 (scaled 0.1 nil)))
      (is (= 10 (scaled 0.5 nil))))))

