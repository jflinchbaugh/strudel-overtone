(ns strudel-overtone.random-song
  (:require [overtone.core :as ov]
            [strudel-overtone.strudel-overtone :refer :all]))

(comment
  (seed! 44) ;; Set a seed for repeatable randomness

  (play!
   :kick (->
          (s :dance-kick)
          (fast 2)
          (gain (choose [0.5 0.2]))
          (pan (srand -0.4 0.4))
          (active (wchoose [[0 0.9] [1 0.1]])))

   :snare (->
           (s [:snare])
           (gain (wchoose [[0.2 0.9] [0.1 0.1]]))
           (pan (srand -0.5 0.5)))

   :hat (->
         (s [:hat])
         (fast 2)
         (gain (srand 0.1 0.3)) ;; random velocity for each hat
         (pan (srand -1 1)))

   :melody (->
            (note (choose [:c3 :e3 :g3 :b3 :c4]))
            (swing (srand 0.0 0.5))
            (s [:ks-stringer])
            (fast (choose [4 8 16])) ;; randomly change the speed
            (gain 0.4)
            (release (srand 0.1 1.0)) ;; random release times
            (pan sine)
            (ribbon 2 2)
            (active true))

   :chopped (->
             (note (choose-n 32 [:c4 :e4 :g4 :b4]))
             (s [:saw])
             (add (choose [-12 0 12]))
             (gain (choose [0.3 0.1]))
             (pan (srand -1 1))
             (distort (srand 0.0 1.0))
             (active (choose [0 0 0 0 0 1]))
             (ribbon 2 2) ;; Loop only the first quarter of the 16-note pattern
             ))

  ;; Example: Capture the first cycle of a random melody and repeat it
  (play! :frozen-lead (-> (note (choose-n 12 [:c3 :e3 :g3 :b3]))
                        (add 12)
                          (s :saw)
                          (ribbon 0 1) ;; Freeze the first 8 random notes
                          (gain 0.3)
                          (lpf (sig-range sine 500 2000)))) ;; LPF still moves!

  ;; Example: Random chord progression with ks-stringer
  (let [chords [(set (ov/chord :c3 :major))
                (set (ov/chord :a3 :minor))
                (set (ov/chord :f3 :major))
                (set (ov/chord :g3 :major))]]
    (play! :chords (-> (note (choose-n 6 chords))
                      (add -12)
                      (swing 0.5)
                       (s :ks-stringer)
                       (ribbon 1 4) ;; Freeze the random 4-chord progression
                       (gain 0.4)
                       (lpf 2000))))

  (stop!)


  )
