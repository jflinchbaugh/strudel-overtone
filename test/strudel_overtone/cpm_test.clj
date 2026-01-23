(ns strudel-overtone.cpm-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest cpm-test
  (testing "cpm updates metro bpm with 4x multiplier"
    (let [bpm-atom (atom 120)]
      (with-redefs [sut/metro (fn [& args]
                                (when (= :bpm (first args))
                                  (reset! bpm-atom (second args))))
                    ov/metro-bpm (fn [_] @bpm-atom)]
        
        ;; Test setting CPM
        (sut/cpm 30)
        (is (= 120 @bpm-atom))
        
        (sut/cpm 60)
        (is (= 240 @bpm-atom))
        
        ;; Test getting CPM
        (is (= 60.0 (double (sut/cpm))))))))
