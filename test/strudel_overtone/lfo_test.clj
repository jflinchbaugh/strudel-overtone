(ns strudel-overtone.lfo-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.pattern :as p]))

(deftest sine-params-test
  (testing "sine-sig with parameters should not throw and return a function"
    (let [s (p/sine-sig 0.5 500 2000)]
      (is (fn? s))
      (is (number? (s 0.0 nil)))
      (is (number? (s 1.0 nil)))))

  (testing "sine-sig with frequency should return a function"
    (let [s (p/sine-sig 2)]
      (is (fn? s))
      (is (number? (s 0.0 nil))))))

(deftest signal-scaling-test
  (testing "sine-sig scaling works as expected"
    (let [s (p/sine-sig 1 100 200)]
      ;; at t=0, sine(0) = 0.5 (scaled to 0..1), so 150
      (is (< (abs (- (s 0.0 nil) 150)) 0.01))
      ;; at t=0.25, sine(2*pi*0.25) = 1, so 0.5 + 0.5*1 = 1.0, so 200
      (is (< (abs (- (s 0.25 nil) 200)) 0.01))
      ;; at t=0.75, sine(2*pi*0.75) = -1, so 0.5 + 0.5*-1 = 0.0, so 100
      (is (< (abs (- (s 0.75 nil) 100)) 0.01)))))

(deftest other-signals-test
  (testing "saw-sig scaling"
    (let [s (p/saw-sig 1 100 200)]
      (is (< (abs (- (s 0.0 nil) 100)) 0.01))
      (is (< (abs (- (s 0.5 nil) 150)) 0.01))
      (is (< (abs (- (s 0.99 nil) 199)) 0.1))))
  
  (testing "tri-sig scaling"
    (let [s (p/tri-sig 1 100 200)]
      (is (< (abs (- (s 0.0 nil) 100)) 0.01))
      (is (< (abs (- (s 0.25 nil) 150)) 0.01))
      (is (< (abs (- (s 0.5 nil) 200)) 0.01))
      (is (< (abs (- (s 0.75 nil) 150)) 0.01))))
  
  (testing "square-sig scaling"
    (let [s (p/square-sig 1 100 200)]
      (is (< (abs (- (s 0.0 nil) 200)) 0.01))
      (is (< (abs (- (s 0.6 nil) 100)) 0.01)))))
