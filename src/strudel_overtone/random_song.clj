(ns strudel-overtone.random-song
  (:require [overtone.core :as ov]
            [strudel-overtone.core :refer :all]))

(comment
  (seed! 42) ;; Set a seed for repeatable randomness

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
            (delay-cycles 8)
            (ribbon 2 2)
            (active true))

  :chopped (->
              (note (choose-n 32 [:c4 :e4 :g4 :b4]))
              (delay-cycles 1)
              #_(mono)
              (s [:saw])
              (add (choose [-12 0 12]))
              (gain (choose [0.3 0.1]))
              (pan (srand -1 1))
              (distort (srand 0.0 1.0))
              (active (choose [0 0 0 0 0 1]))
              (ribbon 1 2) ;; Loop only the first quarter of the 16-note pattern
              ))

  (stop!)

  ;; Example: Capture the first cycle of a random melody and repeat it
  (play! :frozen-lead (-> (note (choose-n 8 [:c3 :e3 :g3 :b3]))
                        (add 12)
                        (s :saw)
                        (ribbon 2 2)
                        (gain (choose-n 2 [1/4 1/4 1/16]))
                        #_(lpf (sig-range sine 500 2000)))

    )


  (play! :test (->
                 (s [:saw])
                 (mono)
                 (note (choose-n 4 [:c3 :e3 :g3 :b3]))
                 (glide 0.1)
                 (ribbon 0 2)
                 (gain (choose-n 8 [1/4 1/2 1/4]))
                 ))


  (stop!)

  ;; Example: Random chord progression with ks-stringer
  (let [chords [(set (ov/chord :c3 :major))
                (set (ov/chord :a3 :minor))
                (set (ov/chord :f3 :major))
                (set (ov/chord :g3 :major))]]
    (play-only!
      :chords (-> (note (choose-n 6 chords))
                      (add -12)
                      #_(swing 0.5)
                       (s :ks-stringer)
                       (ribbon 1 2) ;; Freeze the random 4-chord progression
                       (gain 0.6)
                       (lpf 2000))))

  ;; Example: Super-random chord progression
  ;; We generate a large pool of chords and pick 4 of them to loop
  (let [roots [:c3 :d3 :e3 :f3 :g3 :a3 :b3]
        types (take 10 [:major :minor :minor7 :major7])
        all-chords (for [r roots t types] (set (take 5 (ov/chord r t))))]
    (play! :rand-chords (-> (note (choose-n 2 all-chords))
                            (s :sine)
                            (gain 0.4)
                            (ribbon 0 4) ;; Capture 4 random chords into a loop
                            #_(lpf (sig-range sine 500 5000)))))

  (playing)

  (stop!)

  (ov/stop)

  ;; Example: Acid bassline with glide (portamento)
  (play! :acid (-> (note (choose-n 3 [:c2 :c3 :g2 :a2]))
                   (s :tb303)
                   (adsr 1 1 1 1)
                   #_(mono)
                   (glide 1/3) ;; Slide between notes over 0.3 cycles
                   #_(legato 1.1) ;; Overlap notes slightly for better glide
                   #_(lpf (sig-range sine 200 2000))
                   (resonance 0.8)
                   (gain 0.3)))

  ;; Example: Liquid Lead with smooth blending
  ;; Use s-level 1.0 and legato > 1 to blend notes together
  (play! :liquid (-> (note (choose-n 2 [:c3 :d3 :f3 :g3]))
                     (s [:saw])
                     (mono)
                     (glide 0.2)   ;; 20% of a cycle for the pitch slide
                     (gain 0.3)))

  ;; Example: Plucky Glide with percussive envelope
  (play! :plucky-glide (-> (note (choose-n 4 [:c2 :c2 :c3 :g2]))
                           (s :bd)
                           (mono)
                           (glide 0.1)
                           (gain 0.4)))

  ;; Example: Monophonic lead
  ;; A single synth instance is reused, updating frequency for each note
  (play-only! :mono-lead (-> (note (choose [:c3 :d3 :e3 :g3 :a3]))
                        (s :ks-stringer)
                        (mono)
                        (glide 0.1)
                        (legato 1.0)
                        (adsr 0.05 0 1.0 0.1)
                        (gain 0.2)))

  ;; Example: Monophonic chords (paraphonic)
  ;; Each note in the chord is tracked as a separate monophonic voice

  (def g (atom 0.05))

  (play-only! :mono-chords (-> (note (choose-n 3 [:c2 :e2 :g2 :a2 :c3 :e3 :g3]))
                          (s :tri)
                          (mono)
                          (glide @g)
                          (adsr 0.05 0.1 1.0 0.05)
                          (gain 0.15)
                          (lpf 1000)))

  (stop!)

  (ov/stop)

  (drop 4 (rtake 6 :things (irand 0 10)))

  (play-only! :swoosh (->
                        (note [:a3 :a3 :a3 :a3 :a3])
                        (s :sine)
                        (mono)
                        (glide 1.0)
                        (pan sine)))

  (stop!)

  (ov/stop)

  (play-only! (-> (s [:saw]) (glide 0.5) (gain [0.5 1 0.5]) (mono)))

  .)
