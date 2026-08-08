(ns strudel-overtone.song-synthtok
  (:require [strudel-overtone.core :refer :all]))

(def lpf-a (atom 100))

(let [bp [:b2]
      bi [4 1 1 1 2 2 1 1]
      g [0.6]
      lpf-v 90]
  (cpm 174/8)
  (play-only!
   :b1 (-> (s :saw)
           (note bp)
           (degrees :minor bi)
           (gain g)
           (legato 1/4)
           (lpf lpf-v))
   :b2 (-> (s :sine)
           (note bp)
           (degrees :minor bi)
           (add -12)
           (legato 1/3)
           (gain g)
           (lpf lpf-v))
   :drum (-> (s [:bd :- :- :- :- :bd :- :-])
             (lpf 150))
   :snare (-> (s [:- :- :sd :- :- :- :sd :-]))
   :clap (-> (s [:cp :cp :cp :cp :cp :cp :cp :cp])
             (gain (adsr-sig 0 0 1 1)))
   ))

(play-only! :s (s [:bd]))

(stop!)

((adsr-sig 0 0 1 1) 0 1)

(saw 1)

(reset! lpf-a 200)
