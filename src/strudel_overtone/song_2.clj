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
          (gain 0.5))
   :hat (->
          (s :sine)
          (gain 0.5)
          (note [:- :a0])))

  (play-only!
    #_#_:kick (->
            (s [[:kick :kick] :- [:kick :kick :kick :kick] :-])
            (duck-trigger 1)
            (sustain [2 0.15]))
    #_#_:snare (->
                 (s [:- :snare :- :snare]))
    #_#_:bass (->
                (s :fm)
                (mono)
                (glide 0.2)
                (duck 1)
                (note [:a0 :a0 :b0 :c1])))
    (play!
      :lead (->
              (s :square)
              (note (reverse (chord :a4 :minor)))
              (detune (irand -50 50))
              (ribbon 1 8)
              (swing 0.2)
              (fast 2)
              (gain 0.6)))

    (stop!)

    (scale :a4 :minor)
    ;; => (69 71 72 74 76 77 79 81)

    (chord :a4 :minor7)
    ;; => (69 72 76 79)
    ;; => (69 72 76)

    .)
