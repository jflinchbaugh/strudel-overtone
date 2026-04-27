(ns strudel-overtone.params-group-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]))

(deftest params-grouping-test
  (testing "params function can set multiple parameters at once"
    (let [pat (-> (sut/s :saw)
                  (sut/params {:attack 0.5 :release 2.0 :lpf 500}))
          event (first (:events pat))]
      (is (= 0.5 ((get-in event [:params :attack]) 0 nil)))
      (is (= 2.0 ((get-in event [:params :release]) 0 nil)))
      (is (= 500 ((get-in event [:params :lpf]) 0 nil)))))

  (testing "grouping helpers work"
    (let [pat (-> (sut/s :saw)
                  (sut/adsr 0.1 0.2 0.5 1.0))
          event (first (:events pat))]
      (is (= 0.1 ((get-in event [:params :attack]) 0 nil)))
      (is (= 0.2 ((get-in event [:params :decay]) 0 nil)))
      (is (= 0.5 ((get-in event [:params :s-level]) 0 nil)))
      (is (= 1.0 ((get-in event [:params :release]) 0 nil))))))
