(ns strudel-overtone.nested-test
  (:require [clojure.test :refer :all]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]))

(defn approx= [a b]
  (< (abs (- a b)) 0.01))

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
        (is (= :a (get-in e1 [:params :sound])))
        (is (approx= 0.0 (:time e1)))
        (is (approx= 0.333 (:duration e1)))

        (is (= :b (get-in e2 [:params :sound])))
        (is (approx= 0.333 (:time e2)))
        (is (approx= 0.166 (:duration e2)))

        (is (= :c (get-in e3 [:params :sound])))
        (is (approx= 0.5 (:time e3)))
        (is (approx= 0.166 (:duration e3)))

        (is (= :d (get-in e4 [:params :sound])))
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
        (is (= :d (get-in e4 [:params :sound])))
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
        (is (= #{:e4 :g4} (set (map
                                 #(get-in % [:params :note])
                                 middle-evs)))))))

  (testing "chord helper returns a set of notes playing simultaneously"
    (let [c-chord (sut/chord :c3 :minor7)]
      (is (set? c-chord))
      (is (= #{48 51 55 58} c-chord))
      ;; When used in a pattern vector, it plays simultaneously as 1 step
      (let [pat (sut/note [(sut/chord :c3 :minor7)])
            evs (:events pat)]
        (is (= 4 (count evs)))
        (is (every? #(approx= 0.0 (:time %)) evs))
        (is (every? #(approx= 1.0 (:duration %)) evs)))))

  (testing "chord-seq helper returns a sequence of notes"
    (let [c-seq (sut/chord-seq :c3 :minor7)]
      (is (sequential? c-seq))
      (is (= '(48 51 55 58) (seq c-seq))))))

(deftest cartesian-product-test
  (testing "Combining sets of notes and instruments creates Cartesian product"
    (let [pat (-> (sut/note [:c4])
                  (sut/s [#{:piano :violin}]))]
      ;; Note c4 (0-1). Sound #{:piano :violin} (0-1).
      ;; Expect 2 events: c4 on piano, c4 on violin.
      (is (= 2 (count (:events pat))))
      (let [evs (:events pat)
            sounds (set (map #(get-in % [:params :sound]) evs))]
        (is (= #{:piano :violin} sounds))
        (is (every? #(= :c4 (get-in % [:params :note])) evs)))))

  (testing "Complex Cartesian product"
    (let [pat (-> (sut/note [#{:c4 :e4}])
                  (sut/s [#{:piano :violin}]))]
      ;; Notes: C4, E4. Sounds: Piano, Violin.
      ;; Expect 2x2 = 4 events.
      (is (= 4 (count (:events pat))))
      (let [evs (:events pat)
            combos (set (map
                          (fn [e]
                            [(get-in e [:params :note])
                             (get-in e [:params :sound])])
                          evs))]
        (is (contains? combos [:c4 :piano]))
        (is (contains? combos [:c4 :violin]))
        (is (contains? combos [:e4 :piano]))
        (is (contains? combos [:e4 :violin]))))))

(deftest dynamic-sub-pattern-test
  (testing "trigger-event supports sequential sounds from alt"
    (let [triggered (atom [])]
      (with-redefs [player/trigger-single-event
                    (fn [k ev params beat dur vidx]
                      (swap! triggered conj {:sound (:sound params)
                                             :beat beat
                                             :dur dur}))]
        ;; Cycle 0: alt returns :snare
        (let [pat (sut/s [:kick (sut/alt :snare [:snare :snare])])
              ev (second (:events pat))]
          (sut/trigger-event :test ev 2.0 2.0 0 0 1)
          (is (= [{:sound :snare :beat 2.0 :dur 2.0}] @triggered)))

        (reset! triggered [])
        ;; Cycle 1: alt returns [:snare :snare] -> should subdivide duration into two hits
        (let [pat (sut/s [:kick (sut/alt :snare [:snare :snare])])
              ev (second (:events pat))]
          (sut/trigger-event :test ev 2.0 2.0 0 1 1)
          (is (= [{:sound :snare :beat 2.0 :dur 1.0}
                  {:sound :snare :beat 3.0 :dur 1.0}]
                 @triggered)))))))
