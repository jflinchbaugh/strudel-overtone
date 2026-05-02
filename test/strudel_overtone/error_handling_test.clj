(ns strudel-overtone.error-handling-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.player :as player]
            [overtone.core :as ov]))

(deftest invalid-note-test
  (testing "Playing an invalid note should not crash the player loop"
    ;; trigger-event should now catch the exception and log it instead of throwing
    (let [ev {:params {:note :iii :sound :saw}}]
      (is (nil? (player/trigger-event :test ev (ov/now) 1))))))
