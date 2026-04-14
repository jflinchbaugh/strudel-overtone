(ns strudel-overtone.random-song
  (:require [overtone.core :as ov]
            [strudel-overtone.strudel-overtone :refer :all]))

(comment
  (seed! 42) ;; Set a seed for repeatable randomness

  (play!
   :kick (->
          (s [:kick :kick])
          (gain 0.6)
          (pan (srand -0.4 0.4))) ;; subtle random pan

   :snare (->
           (s [:- :snare])
           (gain (wchoose [[0.5 0.9] [0.2 0.1]])) ;; mostly loud, occasionally quiet
           (pan (srand -0.5 0.5)))

   :hat (->
         (s [:hat])
         (fast 2)
         (gain (srand 0.1 0.3)) ;; random velocity for each hat
         (pan (srand -1 1)))

   :melody (->
            (note (choose [:c3 :e3 :g3 :b3 :c4]))
            #_(swing (srand 0.0 0.5))

            (s [:ks-stringer])

            (fast (choose [16])) ;; randomly change the speed
            (gain 0.4)
            (release (srand 0.1 1.0)) ;; random release times
            (pan sine))

   :chopped (->
             (note (choose-n 16 [:c4 :e4 :g4 :b4]))
             (s [:saw])
             (ribbon 0 0.25) ;; Loop only the first quarter of the 16-note pattern
             (fast 1)        ;; Speed it up to fill the cycle
             (gain 0.3)
             (pan (srand -1 1))))

  (stop!)

  )
