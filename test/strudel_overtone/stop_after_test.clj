(ns strudel-overtone.stop-after-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [overtone.core :as ov]))

(deftest stop-after-test
  (testing "play-loop stops after n cycles"
    (let [player-state (atom {:playing? true 
                              :patterns {:p1 (-> (sut/s [:bd]) (sut/stop-after 1))} 
                              :loops #{:p1}})
          mock-calls (atom [])]
      (with-redefs [player/player-state player-state
                    player/trigger-event (fn [& _] nil)
                    ov/apply-by (fn [_ms _func args]
                                  (swap! mock-calls conj args))]
        
        ;; First call to play-loop at beat 0, start-beat 0
        ;; elapsed-cycles = 0.0. stop-cycles = 1.0. Should play.
        (player/play-loop :p1 0.0 0.0)
        
        ;; Should have scheduled the next call to play-loop at beat 4
        (is (= 1 (count @mock-calls)))
        (is (= [:p1 4.0 0.0] (first @mock-calls)))
        
        ;; Second call to play-loop at beat 4, start-beat 0
        ;; elapsed-cycles = (4 - 0) / 4 = 1.0
        ;; stop-cycles = 1.0. Should stop.
        (reset! mock-calls [])
        (player/play-loop :p1 4.0 0.0)
        
        ;; Should NOT have scheduled another call
        (is (empty? @mock-calls))
        ;; Should have removed the pattern and loop
        (is (not (contains? (:patterns @player-state) :p1)))
        (is (not (contains? (:loops @player-state) :p1))))))

  (testing "stops after fractional cycles if play-loop resolution allows"
    (let [player-state (atom {:playing? true 
                              :patterns {:p1 (-> (sut/s [:bd]) 
                                                 (sut/fast 2) 
                                                 (sut/stop-after 0.5))} 
                              :loops #{:p1}})
          mock-calls (atom [])]
      (with-redefs [player/player-state player-state
                    player/trigger-event (fn [& _] nil)
                    ov/apply-by (fn [_ms _func args]
                                  (swap! mock-calls conj args))]
        ;; fast 2 means cycles = 2, so cycle-dur = 4/2 = 2 beats.
        
        ;; First call at beat 0, start-beat 0
        ;; elapsed-cycles = 0.0. stop-cycles = 0.5. Should play.
        (player/play-loop :p1 0.0 0.0)
        
        ;; Should have scheduled next call at beat 2
        (is (= 1 (count @mock-calls)))
        (is (= [:p1 2.0 0.0] (first @mock-calls)))

        ;; Second call at beat 2, start-beat 0
        ;; elapsed-cycles = 2/4 = 0.5
        ;; stop-cycles = 0.5. Should stop.
        (reset! mock-calls [])
        (player/play-loop :p1 2.0 0.0)
        
        (is (empty? @mock-calls))
        (is (not (contains? (:patterns @player-state) :p1)))))))
