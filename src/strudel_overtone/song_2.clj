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

  (play-only!
   :kick (->
          (s [[:kick :kick] :- [:kick :kick :- :kick :kick] :-])
          (duck-trigger 1)
          (lpf 500)
          (gain 0.5)
          (sustain [2 0.15]))
   #_#_:hihat (->
           (s [:hh])
           (duck 1)
           (gain 0.5)
           (fast 8)
           (lpf 5000))
   #_#_:snare (->
           (s [:- :snare :- :snare]))
   :bass (->
          (s :fm)
          (mono)
          (glide 0.2)
          (duck 1)
          (note [:a0 :a0 :b0 :c1]))
   :lead (->
           (s :square)
           (note (choose-n 4 (chord :a4 :minor7)))
           (fast 2)
           (pan [-0.75 0.75 -0.75 0.75])
           (ribbon 1 2)
           (swing 0.1)
           (gain 0.2)))

  (stop!)

  (scale :a4 :minor)
    ;; => (69 71 72 74 76 77 79 81)

  (chord :a4 :minor7)
    ;; => (69 72 76 79)
    ;; => (69 72 76)

  .)
