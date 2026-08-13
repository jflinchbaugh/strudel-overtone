(ns strudel-overtone.sound-design-showcase
  "Showcase of sound design DSL shortcuts (acid, drive, space, lfo)."
  (:require [strudel-overtone.core :refer :all]))

(comment
  ;; Start playback server/REPL as instructed in README.md

  ;; 1. Acid Synth Line
  ;; Uses (acid cutoff-hz res-amount env-depth) to get rich 303 filter sweeps
  (play! :acid-bass
         (-> (note [:c2 :c2 :eb2 :f2 :g2 :bb2])
             (s :tb303)
             (mono)
             (glide 0.05)
             (acid 600 0.05 4000)
             (gain 0.7)))

  ;; 2. Drive & Bitcrushing
  ;; Uses (drive distort-amount crush-amount) for distortion and bitcrushing
  (play! :grit-lead
         (-> (note [:c4 :eb4 :g4 :bb4])
             (s :saw)
             (drive 0.6 0.2)
             (lpf 3000)
             (gain 0.5)))

  ;; 3. Space & Reverb / Echo
  ;; Uses (space room-mix delay-sec delay-repeats) for quick spatial fx
  (play! :ambient-keys
         (-> (note [:c4 :g4 :d5 :a5])
             (s :sine)
             (perc 0.01 0.4)
             (space 0.6 0.25 6)
             (gain 0.4)))

  ;; 4. LFO Modulation
  ;; Uses (lfo param-key rate-hz depth-val) to modulate filter frequencies
  (play! :lfo-pad
         (-> (note [#{:c3 :g3 :c4}])
             (s :saw)
             (lfo :lpf 2 1500)
             (lfo :pan 0.5 0.8)
             (gain 0.5)))

  ;; Stop playback
  (stop!)


  )
