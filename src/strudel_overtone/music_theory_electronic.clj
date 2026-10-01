(ns strudel-overtone.music-theory-electronic
  (:require
   [strudel-overtone.core :refer :all]
   [overtone.core :as ov]))

(comment

  (play-only!
   :power-chord-base (-> (s :tri)
                         (note [:e2 :g2 [:g2 :a2] :a2])
                         (mono)
                         (add -12))
   :power-chord-fifth (-> (s :tri)
                          (note [:b2 :d3 [:d3 :e3] :e3])
                          (perc 0.01 1.7)
                          (add -12))
   #_#_:power-chord-base (-> (s :tri)
                             (note [:e3 :g3 [:g3 :a3] :a3])
                             (add -12))
   )

  (stop!)

  (play-only!
   :lead (->
          (s :tri)
          (note [[:c4 :a3] [:g3 :a3] [:f3 :g3] :c4]))
   :fifths (->
            (s :tri)
            (note [[:g4 :e4] [:d4 :e4] [:c4 :d4] :g4]))
   )

  (play-only!
   :lead (->
          (s :tri)
          (note (alt :f3 :c4 :g3))
          (degrees :major [[5 3] [2 3] [1 2] 5]))
    :fifths (->
          (s :tri)
          (note (alt :c4 :g4 :d4))
          (degrees :major [[5 3] [2 3] [1 2] 5]))
    )

  (scale :a4 :major)
  ;; => (69 71 73 74 76 78 80 81)

  (chord-seq :a4 :major)
  ;; => (69 73 76)


  (stop!)

  (play-only!
   :lead (->
          (s :square)
          (note :c3)
          (degrees :major [1 5 3 (legato 4 2) :- [6 5] 3 2])))

  (play-only!
   :out-of-key (->
          (s :square)
          (note :c3)
          (degrees :major [1 5 3 (legato 4 2) :- [(add 5 1) 5] 3 2])))


  (play-only!
   :lead (-> (s :square)
             (note :c3)
             (degrees :major [1 5 [4 3] 5 [4 3] 2])))


  (play-only!
   :lead (-> (s :square)
             (note :c3)
             (degrees :major [1 5 [4 3] 5 [4 3] 2 1])))

  (stop!)

  ;; mary had a little lamb

  (play-only!
   :lead (-> (s :saw)
             (note :c3)
             (slow 4)
             (degrees :major [3 2 1 2 3 3 (legato 3 2) :-
                              2 2 (legato 2 2) :- 3 5 (legato 5 2) :-
                              3 2 1 2 3 3 3 3 2 2 3 2 (legato 1 4) :- :- :-])))


  (stop!)

  )

