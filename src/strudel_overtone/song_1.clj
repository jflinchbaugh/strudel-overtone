(ns strudel-overtone.song-1
  (:require [overtone.core :refer :all]
            [strudel-overtone.strudel-overtone :refer :all]))

(comment
  (cpm)

  (slide-cpm 32 8)

  (playing)

  (play!

   :plucks (->
            (note (chosen-from (chord :c6 :minor7) 3))
            (fast 4)
            (s :ks-stringer)
            (distort 0.7)
            #_(echo-delay 0.02)
            (crush 0.9)
            (distort 0.5)
            (release 0.01)
            (room 0.5)
            (pan-hz 1)
            (pan-depth 0.9)
            (duck-trigger 1)
            (swing [0/4])
            (gain 0.3)))

  (play!
   :kick (->
           (s [#{[:kick :- :-] :dub-kick} :-
              [:- #{[:kick :- :-] :dub-kick}] :-])
          (note [:d2 :c2])
          (gain 1)
          (duck-trigger 1))
   :snare (->
           (s [:- :snare :- :snare])
           (gain [0.6 0.8])
           (note (chosen-from (chord :e3 :minor) 2)))
   :hat (->
         (s (map (fn [n] (cons  n [:- :-])) (take 8 (cycle [:hat]))))
         (gain 0.6)
         (swing 0/3)
         (duck 0.9))
   :pad (->
         (note [:c2 [:b2 :b3 :d2]])
         (s [#{:mooger}])
         (resonance 3)
         (distort 0.5)
         (add [-24 -12])
         (attack 1.2)
         (release 0.01)
         (gain [0.3 0.3])
         (s-level 1)
         (duck 0.8)))

  (stop! :hat)

  (stop! :snare)

  (stop! :kick)

  (stop! :pad)

  (stop! :plucks)

  (stop!)


  .)
