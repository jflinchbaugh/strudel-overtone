(ns strudel-overtone.play-only-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]))

(deftest play-only-test
  (testing "play-only! removes patterns not in the new set"
    (reset! sut/player-state
            {:playing? true
             :patterns {:p1 (sut/s [:bd]) :p2 (sut/s [:hh])}
             :loops #{:p1 :p2}})
    
    (sut/play-only! :p2 (sut/s [:sn]))
    
    (let [state @sut/player-state]
      (is (= #{:p2} (set (keys (:patterns state)))))
      (is (= "sn" (get-in (first (get-in state [:patterns :p2 :events])) [:params :sound])))))

  (testing "play-only! with single pattern defaults to :main and clears others"
    (reset! sut/player-state
            {:playing? true
             :patterns {:p1 (sut/s [:bd]) :p2 (sut/s [:hh])}
             :loops #{:p1 :p2}})
    
    (sut/play-only! (sut/s [:clap]))
    
    (let [state @sut/player-state]
      (is (= #{:main} (set (keys (:patterns state)))))
      (is (= "clap" (get-in (first (get-in state [:patterns :main :events])) [:params :sound]))))))
