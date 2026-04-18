(ns strudel-overtone.load-sample-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [strudel-overtone.samples :as samples]
            [overtone.core :as ov]))

(deftest load-sample-return-value-test
  (testing "load-sample! returns the name when successful"
    (with-redefs [ov/load-sample (constantly {:id 1 :duration 1.0})]
      (is (= :my-sample (sut/load-sample! :my-sample "fake/path")))))

  (testing "load-sample! returns nil when it fails"
    (with-redefs [ov/load-sample (fn [_] (throw (Exception. "fail")))]
      (is (nil? (sut/load-sample! :fail-sample "fake/path"))))))

(deftest sample-info-test
  (let [buf {:duration 10.0 :n-channels 2 :rate 44100.0 :path "path" :size 441000}]
    (with-redefs [samples/samples (atom {:test buf})
                  samples/sample-slices (atom {})]
      (testing "info for regular sample"
        (let [info (sut/sample-info :test)]
          (is (= :sample (:type info)))
          (is (= 10.0 (:duration info)))))

      (testing "info for slice"
        (sut/slice-sample! :test-slice :test 0.1 0.2)
        (let [info (sut/sample-info :test-slice)]
          (is (= :slice (:type info)))
          (is (= :test (:source info)))
          (is (= 1.0 (:duration info)))
          (is (= 10.0 (:full-duration info)))))

      (testing "non-existent sample"
        (is (nil? (sut/sample-info :nope)))))))

