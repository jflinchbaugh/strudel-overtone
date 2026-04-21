(ns strudel-overtone.freesound-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.samples :as samples]
            [overtone.core :as ov]))

(deftest load-freesound-return-value-test
  (testing "load-freesound! returns the name when successful"
    (with-redefs [ov/freesound (constantly {:id 1 :duration 1.0})]
      (is (= :my-fs (sut/load-freesound! :my-fs 12345)))))

  (testing "load-freesound! returns nil when it fails"
    (with-redefs [ov/freesound (fn [_] (throw (Exception. "fail")))]
      (is (nil? (sut/load-freesound! :fail-fs 12345))))))
