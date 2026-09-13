(ns strudel-overtone.core-entry-test
  (:require [clojure.test :refer [deftest testing is]]
            [strudel-overtone.core :as sut]
            [overtone.core :as ov]))

(deftest reload-and-main-test
  (testing "reload! reloads namespaces and refers into target namespace"
    (let [dummy-ns (create-ns 'strudel-overtone.dummy-target)]
      (with-redefs [clojure.core/require (constantly nil)]
        (is (= :reloaded (sut/reload! dummy-ns)))
        (is (some? (ns-resolve dummy-ns 'play!)))
        (is (some? (ns-resolve dummy-ns 'note))))))

  (testing "reload! with no args defaults to current namespace"
    (with-redefs [clojure.core/require (constantly nil)]
      (is (= :reloaded (sut/reload!)))))

  (testing "-main connects server and logs readiness"
    (let [connected (atom false)]
      (with-redefs [ov/connect-server (fn [] (reset! connected true))]
        (sut/-main)
        (is (true? @connected))))))
