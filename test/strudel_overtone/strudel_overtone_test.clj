(ns strudel-overtone.strudel-overtone-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]))

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
