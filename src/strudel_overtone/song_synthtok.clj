(ns strudel-overtone.song-synthtok
  (:require [strudel-overtone.core :refer :all]))

(def lpf-a (atom 100))

(let [bp [:b2]
      bi [4 1 1 1 2 1 1 1]
      g [0.5]
      lpf-v (sine 1/4 300 500) #_(fn [_ _] @lpf-a)]
  (cpm 174/4)
  (play-only!
   #_#_:b1 (-> (s :saw)
           (note bp)
           (degrees :minor bi)
           (gain g)
           (legato 1/4)
           (lpf lpf-v))
   #_#_:b2 (-> (s :sine)
           (note bp)
           (degrees :minor bi)
           (add -4)
           (gain g)
           (lpf lpf-v))
   #_#_:drum (-> (s [:bd :- :- :- :- :bd :- :-])
             (lpf 150))
   #_#_:snare (-> (s [:- :- :sd :- :- :- :sd :-]))
   :clap (-> (s [:cp :cp :cp :cp :cp :cp :cp :cp])
             (gain (saw 1/8 0 1)))
   ))

(stop!)

((adsr-sig 0 0 1 1) 0 0.5)

(saw 1)

(reset! lpf-a 200)
