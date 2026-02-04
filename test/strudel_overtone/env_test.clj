(ns strudel-overtone.env-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]
            [overtone.core :as ov]))

(deftest synth-lookup-test
  (testing "get-synth-name resolves aliases"
    (is (= "saw-adsr" (#'sut/get-synth-name "saw" {})))
    (is (= "bd-adsr" (#'sut/get-synth-name "bd" {})))
    (is (= "sd-adsr" (#'sut/get-synth-name "sd" {}))))

  (testing "get-synth-name handles env param"
    (is (= "bd-perc" (#'sut/get-synth-name "bd" {:env "perc"})))
    (is (= "saw-perc" (#'sut/get-synth-name "saw" {:env "perc"})))
    (is (= "sine-perc" (#'sut/get-synth-name "sine" {:env "perc"}))))

  (testing "resolve-synth finds existing synths"
    (is (var? (#'sut/resolve-synth "kick-adsr")))
    (is (var? (#'sut/resolve-synth "saw-adsr")))
    (is (var? (#'sut/resolve-synth "saw-perc")))
    (is (nil? (#'sut/resolve-synth "non-existent-synth")))))

(deftest env-param-test
  (testing "env function sets param"
    (let [pat (-> (sut/s [:saw-synth]) (sut/env :perc))]
      (is (= "perc" (get-in (first (:events pat)) [:params :env])))))

  (testing "trigger-event calls perc synth"
    (let [mock-calls (atom [])]
      (with-redefs [ov/metro-bpm (constantly 120)
                    sut/metro (constantly 0)
                    ov/apply-at (fn [ms func & args] (swap! mock-calls conj {:func func :args args}))
                    sut/saw-perc (fn [& args] args)] ;; Mock synth var

        ;; We need to make sure sut/saw-perc is resolved to our mock
        ;; But resolve-synth uses ns-resolve on the real namespace.
        ;; So redefing sut/saw-perc might not be enough if resolve-synth looks up the Var directly.
        ;; However, resolve-synth returns the Var. apply-at calls the Var.
        ;; If we redef the Var root, it should work.

        (let [pat (-> (sut/note [:c4])
                      (sut/s [:saw-synth])
                      (sut/env :perc))
              ev (first (:events pat))]

          ;; But wait, resolve-synth returns the Var.
          ;; If I verify that resolve-synth returns the correct Var name, that's enough for unit testing logic.
          ;; Integration test:
          (with-redefs [sut/resolve-synth (fn [name]
                                            (if (= name "saw-synth-perc")
                                              #'sut/saw-perc
                                              nil))]
             (sut/trigger-event ev 0 1)
             (let [synth-call (second @mock-calls)]
               (is (= #'sut/saw-perc (:func synth-call))))))))))
