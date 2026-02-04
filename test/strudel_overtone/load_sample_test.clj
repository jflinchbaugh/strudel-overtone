(ns strudel-overtone.load-sample-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest load-sample-return-value-test
  (testing "load-sample! returns the name when successful"
    (with-redefs [ov/load-sample (constantly {:id 1 :duration 1.0})]
      (is (= :my-sample (sut/load-sample! :my-sample "fake/path")))))

  (testing "load-sample! returns nil when it fails"
    (with-redefs [ov/load-sample (fn [_] (throw (Exception. "fail")))]
      (is (nil? (sut/load-sample! :fail-sample "fake/path"))))))
