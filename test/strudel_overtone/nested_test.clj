(ns strudel-overtone.nested-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.strudel-overtone :as sut]))

(defn approx= [a b]
  (< (Math/abs (- a b)) 0.01))

(deftest nested-vector-test
  (testing "Nested vectors split the duration"
    (let [pat (sut/s [:a [:b :c] :d])]
      ;; Structure:
      ;; :a -> 0.0 to 0.333 (1/3)
      ;; [:b :c] -> 0.333 to 0.666 (1/3)
      ;;    :b -> 0.333, dur 1/6
      ;;    :c -> 0.5, dur 1/6
      ;; :d -> 0.666 to 1.0 (1/3)
      
      (is (= 4 (count (:events pat))))
      
      (let [evs (:events pat)
            [e1 e2 e3 e4] evs]
        (is (= "a" (get-in e1 [:params :sound])))
        (is (approx= 0.0 (:time e1)))
        (is (approx= 0.333 (:duration e1)))

        (is (= "b" (get-in e2 [:params :sound])))
        (is (approx= 0.333 (:time e2)))
        (is (approx= 0.166 (:duration e2)))

        (is (= "c" (get-in e3 [:params :sound])))
        (is (approx= 0.5 (:time e3)))
        (is (approx= 0.166 (:duration e3)))

        (is (= "d" (get-in e4 [:params :sound])))
        (is (approx= 0.666 (:time e4)))
        (is (approx= 0.333 (:duration e4))))))
  
  (testing "Deeply nested vectors"
    (let [pat (sut/s [:a [:b [:c :d]]])]
      ;; :a -> 0.0, dur 0.5
      ;; [...] -> 0.5, dur 0.5
      ;;    :b -> 0.5, dur 0.25
      ;;    [:c :d] -> 0.75, dur 0.25
      ;;       :c -> 0.75, dur 0.125
      ;;       :d -> 0.875, dur 0.125
      
      (is (= 4 (count (:events pat))))
      (let [evs (:events pat)
            e4 (last evs)]
        (is (= "d" (get-in e4 [:params :sound])))
        (is (approx= 0.875 (:time e4)))
        (is (approx= 0.125 (:duration e4)))))))

(deftest chord-test
  (testing "Sets create simultaneous events (chords)"
    (let [pat (sut/note [:c4 #{:e4 :g4} :b4])]
      ;; :c4 -> 0.0, dur 0.33
      ;; #{:e4 :g4} -> 0.33, dur 0.33
      ;;    :e4 -> 0.33, dur 0.33
      ;;    :g4 -> 0.33, dur 0.33
      ;; :b4 -> 0.66, dur 0.33
      
      (is (= 4 (count (:events pat))))
      
      (let [evs (:events pat)
            sorted-evs (sort-by :time evs)
            e1 (first sorted-evs)
            middle-evs (filter #(approx= 0.333 (:time %)) evs)
            e4 (last sorted-evs)]
            
        (is (= :c4 (get-in e1 [:params :note])))
        (is (approx= 0.333 (:duration e1)))
        
        (is (= 2 (count middle-evs)))
        (is (= #{:e4 :g4} (set (map #(get-in % [:params :note]) middle-evs))))
        (is (every? #(approx= 0.333 (:duration %)) middle-evs))
        
        (is (= :b4 (get-in e4 [:params :note])))
        (is (approx= 0.666 (:time e4))))))

  (testing "simul helper works"
    (let [pat (sut/note [:c4 (sut/simul [:e4 :g4]) :b4])]
      (is (= 4 (count (:events pat))))
      (let [evs (:events pat)
            middle-evs (filter #(approx= 0.333 (:time %)) evs)]
        (is (= 2 (count middle-evs)))
        (is (= #{:e4 :g4} (set (map #(get-in % [:params :note]) middle-evs))))))))