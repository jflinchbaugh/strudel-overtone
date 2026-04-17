(ns strudel-overtone.song-1
  (:require [overtone.core :as ov]
            [strudel-overtone.strudel-overtone :refer :all]))

(comment
  (cpm)

  (glide-cpm 32 8)

  (playing)
  (stop!)
  (play!
   :plucks (->
            (note (choose-n 1 (ov/chord :c6 :minor7)))
            (add 0)
            (fast 4)
            (s :ks-stringer)
            #_(distort 0.7)
            #_(echo-delay 0.02)
            #_(crush 0.9)
            #_(distort 0.5)
            #_(release 0.01)
            #_(room 0.5)
            #_(pan-hz 1)
            #_(pan-depth 0.9)
            (duck-trigger 1)
            #_(swing [0/4])
            #_(gain 0.3)))

  (play-only!
   :kick (->
          (s [#{[:kick :- :-] :dub-kick} :-
              [:- #{[:kick :- :-] :dub-kick}] :-])
          (note [:d2 :c2])
          (gain 1)
          (duck-trigger 1))
   :snare (->
           (s [:- [:snare :snare] :- :snare])
           (slow 1)
           (gain [0.6 0.8])
           (note (choose-n 2 (ov/chord :e3 :minor))))
   :hat (->
         (s (map (fn [n] (cons  n [:- :-])) (take 8 (cycle [:hat]))))
         (gain 0.6)
         (swing 0/3)
         (duck 0.9))
   :pad (->
         (note [:c3 [:b2 :d3 :d2]])
         (mono)
         (fast 1/2)
         (s [#{:mooger}])
         (resonance 3)
         (distort 0.5)
         (add [-12 -12])
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

  (play-only! :pad (->
                     (note [:c2 [:b2 :b3 :d2]])
                     (fast 2)
                     (mono)
                     (s [:mooger])
                     (resonance 3)
                     (distort 0.5)
                     (add [-12 -12])
                     (attack 1.2)
                     (release 0.01)
                     (gain [0.3 0.3])
                     (s-level 1)
                     (duck 0.8)))

  (playing)

  (stop!)

  .)
