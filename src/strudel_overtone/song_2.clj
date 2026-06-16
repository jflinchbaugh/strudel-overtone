(ns strudel-overtone.song-2
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

(comment

  (cpm 160/4)

  (def-additive! :add [1 0.1 0.5])

  (play-only!
   :bass (->
          (s :fm)
          (note (choose-n 8 (chord :a0 :minor7)))
          (ribbon 1 4)
          (duck 1)
          (gain 0.5))
   :hat (->
         (s :sine)
         (gain 0.5)
         (note [:- :a0])))

  (glide-cpm 174/4 8)

  (stop!)

  (play-only!
    :kick (->
            (s [[:kick] :- [:- :kick] :-])
            (duck-trigger 1)
            (lpf 500)
            (gain 0.5))

    :hihat (->
             (s [:hh :hh :hh :hh :hh :hh :hh :hh])
             (degrade 2/8)
             (duck 1)
             (gain 0.2)
             (fast 1)
             (lpf 5000))

    :snare (->
             (s [:- :snare :- :snare]))

    :bass (-> (s :sine)
            (note (choose-n 6 [:a0 :c1 :b0 :c1 :a0 :c1]))
            (add 2)
            (degrade 3/5)
            #_(mono)
            (glide 0.1)
            (distort 0.4)
            (gain [0.6 0.9])
            (tremolo-hz 4)
            (vibrato 4)
            (ribbon 2 1)
            (duck 1))

    :lead (->
            (s :supersaw)
            (note (choose-n 4 (chord :g4 :minor)))
            (perc 0.001 0.6)
            (fast 4)
            (pan [-0.75 0.75 -0.75 0.75])
            (degrade 1/8)
            (ribbon 2 16)
            (gain 0.2))
    )

  (play-only!
    :lead (->
           (s :add)
           (note (alt :d3 :d5))
           (degrees :pentatonic [1 [2 2] 3 4 5 6 7 8 9 10])
           (perc 0.001 0.1)
           (pan (overlay [-0.75 0.75 -0.75 0.75]))
           (gain 0.3)))

  (stop!)

  (ov/stop)

  (play-only!
    :lead (->
            (s :supersaw)
            (note :a6)
            (degrees :major [0 1 1 1])))

  .)
