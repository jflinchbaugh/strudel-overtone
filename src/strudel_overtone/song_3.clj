(ns strudel-overtone.song-3
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

(comment
  (cpm 30)

  (play-only!
   :bass (->
          (s :fm)
          (distort 0.5)
          (note :c2)
          (slow 4)
          (legato 5/4)
          (degrees :minor [6 6 6 6 6 6 6 [8 5] 5 5 5 5 5 5 5 [8 6]])))

  (stop!)

  (ov/stop)

  (play-only!
   #_#_:snare (->
           (s [:snare [:snare :snare] :snare [:snare :snare]])
           (legato 1/4)
           (swing 4/8)
           )
   #_#_:dnb (->
         (s [[:bd :bd] :snare [:- [:bd :bd]] :snare])
         (note :d2)
         (gain 1.5)
         (fast 1)
         (duck-trigger 1))
   #_#_:bass (->
          (s [:bd :bd])
          (note :d1)
          (legato 1/2)
          )
   #_#_:rg (->
          (s :supersaw)
          (note :d2)
          (swing 1/8)
          (degrees :minor [8 5 5 8 [6 6] [6 6] [5 5] [5 5]])
          (legato (overlay [6/4 4/4 4/4 6/4 1 1 1 1]))
          (gain 1)
          (distort (overlay [3/4 0.5 0.5 3/4 0.5 0.5 0.5 0.5]))
          (duck 1))
   #_#_:pad (->
         (s :sine)
         (note :d1)
         (swing 1/2)
         (slow 2)
         (degrees :minor [1 2 1 [1 2 3 2]])
         (gain 3)
         (duck 1)))


  (cpm 22)

  (play-only!
   :strum (->
           (s :ks-stringer)
           (echo 0.02 3)
           (gain 0.25)
           (adsr 0.07 0.05 0.3 0)
           (note :d#4)
           (slow 2)
           (legato 1.1)
           (degrees :major [#{1 3} #{1 3} #{1 3} #{1 3} #{2 4} #{2 4} #{2 4} #{2 4}]))
   :string (->
            (s :ks-stringer)
            (note :d#4)
            (adsr 0.07 0.05 0.3 0.1)
            (gain 1)
            (slow 2)
            (degrees :major [:- :- :- :- :- :- [#{1} #{2}] [:- #{4}]])
            (legato 3))
   :snare (->
           (s [:- :snare :- :snare])
           (gain 0.5)
           (legato 1/4))
   :bd (->
           (s [:bd :- :bd :-])
           (gain 0.5)
           )
   )

  (chord :d4 :minor)

  (stop!)

 )
