(ns strudel-overtone.tutorial-walkthrough
  "Comprehensive, hands-on tutorial walkthrough for strudel-overtone.
   Evaluate forms block-by-block in your REPL (e.g. CIDER or Calva)
   to hear and experiment with each feature."
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

(comment

  ;; =========================================================================
  ;; 1. GETTING STARTED & TEMPO CONTROL
  ;; =========================================================================

  ;; Set the global tempo in Cycles Per Minute (CPM).
  ;; 1 cycle = 4 metronome beats. 30 CPM = 120 BPM.
  (cpm 30)

  ;; Smoothly transition tempo over 4 cycles:
  (glide-cpm 35 4)

  ;; Stop all active loops whenever needed:
  (stop!)

  ;; Inspect currently running loops:
  (playing)

;; =========================================================================
  ;; 2. BASIC RHYTHMS & SUBDIVISIONS
  ;; =========================================================================

  ;; -------------------------------------------------------------------------
  ;; Pattern Names
  ;; Each running loop is identified by a unique keyword name (e.g. :drums,
  ;; :kick, :bass).
  ;; - Calling play! with a new name adds a parallel layer running in sync.
  ;; - Calling play! with an existing name hot-swaps that pattern seamlessly
  ;;   at the cycle boundary without restarting the metronome.
  ;; - (stop! :name) stops only that specific loop, while (stop!) stops all.
  ;; -------------------------------------------------------------------------

  ;; Vectors divide the cycle evenly into steps.
  ;; 4 steps: kick on 1 and 3, snare on 2 and 4
  (play!
   :drums (s [:kick :snare :kick :snare]))

  ;; Subdividing steps with nested vectors:
  ;; Step 2 is split into two fast snares.
  (play!
   :drums (s [:kick [:snare :snare] :kick :snare]))

  ;; Silence with rests (:-)
  (play!
   :hats (-> (s [:- :hat :- :hat])
             (gain 0.6)))

  ;; Chords / simultaneous hits with sets #{...}
  (play!
   :drums (s [#{:kick :hat} :hat #{:snare :hat} :hat]))

  (stop!)

  ;; =========================================================================
  ;; 3. EUCLIDEAN RHYTHMS (euclid)
  ;; =========================================================================

  ;; (euclid hits steps [fill-token] [rest-token] [rotation])
  ;; 3 hits distributed over 8 steps (Tresillo rhythm):
  (play!
   :kick (s (euclid 3 8 :dance-kick)))

  ;; 5 hits over 8 steps, rotated by 1 step:
  (play!
   :hats (-> (s (euclid 5 8 :hat :- 1))
             (gain 0.5)))

  ;; 4 hits over 12 steps:
  (play!
   :clap (-> (s (euclid 4 12 :clap))
             (gain 0.7)
             (room 0.3)))

  (stop!)

  ;; =========================================================================
  ;; 4. MELODIC SYNTHS & SCALES
  ;; =========================================================================

  ;; Play a melody with built-in synth (:saw, :sine, :square, :tri, :tb303, etc.)
  (play!
   :lead (-> (note [:c4 :eb4 :g4 :bb4])
             (s :saw)
             (lpf 2400)
             (gain 0.6)))

  ;; Modal scale degrees (degrees scale-key degree-vector)
  (play!
   :dorian-bass (-> (note :c2)
                    (degrees :dorian [1 3 4 5 7 5 4 3])
                    (s :mooger)
                    (lpf 900)
                    (gain 0.8)))

  ;; Chords using chord helper (returns a set, playing simultaneously):
  (play!
   :chords (-> (note [(chord :c3 :minor7)
                      (chord :f3 :minor7)
                      (chord :g3 :7)])
               (s :saw)
               (slow 2)
               (room 0.4)
               (gain 0.5)))

  ;; Use chord-seq when you want an arpeggiated sequence of chord notes:
  (play!
   :arpeggio (-> (note (chord-seq :c4 :minor7))
                 (s :sine)
                 (fast 4)
                 (gain 0.6)))

  (stop!)

  ;; =========================================================================
  ;; 5. CUSTOM ADDITIVE SYNTHESIS (def-additive!)
  ;; =========================================================================

  ;; Build your own harmonic synths with amplitude ratios!
  ;; Drawbar organ (all integer harmonics):
  (def-additive! :tut-organ [1.0 0.8 0.6 0.4 0.3 0.1])

  ;; Metallic chime (inharmonic partials via golden ratio step):
  (def-additive! :tut-chime [1.0 0.7 0.5 0.3] :step 1.618)

  (play!
   :additive-demo (-> (note [:c4 :eb4 :g4 :c5])
                      (s :tut-organ)
                      (adsr 0.05 0.2 0.6 0.2)
                      (room 0.4)
                      (gain 0.5)))

  (play!
   :bells (-> (note [:- :g5 :- :d6])
              (s :tut-chime)
              (perc 0.001 0.7)
              (echo 0.25 4)
              (gain 0.4)))

  (stop!)

  ;; =========================================================================
  ;; 6. ENVELOPES & SOUND DESIGN (ADSR, Perc, Acid, Drive, Space)
  ;; =========================================================================

  ;; Percussive pluck envelope on saw:
  (play!
   :pluck (-> (note [:c4 :eb4 :g4 :c5])
              (s :saw)
              (env :perc)
              (perc 0.005 0.15)
              (gain 0.7)))

  ;; Filter envelope sweep (acid style):
  (play!
   :acid-bass (-> (note [:c2 :c3 :eb2 :g2])
                  (s :tb303)
                  (lpf 350)
                  (lpf-env 4000)
                  (lpf-adsr 0.01 0.15 0.1 0.1)
                  (resonance 0.85)
                  (gain 0.7)))

  ;; Sound design shortcuts:
  ;; (acid cutoff res depth) - instant squelchy 303 filter envelope:
  (play!
   :quick-acid (-> (note [:c2 :eb2 :f2 :g2])
                   (s :tb303)
                   (acid 400 0.85 3500)
                   (gain 0.7)))

  ;; (drive distort crush) - warm saturation and crunchy bitcrushing:
  (play!
   :gritty-drums (-> (s [:kick :hat :snare :hat])
                     (drive 0.6 0.3)
                     (gain 0.8)))

  ;; (space room delay repeats) - cavernous reverb and tempo-synced echo:
  (play!
   :ambient-keys (-> (note [:c4 :g4 :d5 :a5])
                     (s :sine)
                     (perc 0.01 0.4)
                     (space 0.6 0.25 5)
                     (gain 0.5)))

  (stop!)

  ;; =========================================================================
  ;; 7. MONOPHONIC MODE, GLIDES & DUCKING
  ;; =========================================================================

  ;; Kick that sends ducking triggers to the ducking bus:
  (play!
   :kick (-> (s [:kick :- :kick :-])
             (duck-trigger 1)))

  ;; Monophonic bassline that glides between notes and ducks behind kick:
  (play!
   :mono-glide (-> (note [:c2 :c2 :eb2 :g2 :f2 :-])
                   (s :tb303)
                   (mono)
                   (glide 0.06)
                   (lpf 900)
                   (duck 0.8)       ; dips volume by 80% on kick
                   (gain 0.7)))

  (stop!)

  ;; =========================================================================
  ;; 8. EXPRESSIVE INLINE MODIFICATIONS
  ;; =========================================================================

  ;; Attach dynamics, slides, or filters directly to individual tokens,
  ;; vector groups, or chords:
  (play!
   :expressive (-> (note [:c3
                          (-> [:eb3 :g3 :bb3] (gain 0.4) (lpf 800))
                          :c4
                          (-> :d4 (gain 1.0) (glide 0.1) (lpf 3000))])
                   (s :saw)
                   (mono)
                   (room 0.3)))

  (stop!)

  ;; =========================================================================
  ;; 9. TIME MODIFIERS & MULTI-CYCLE ARRANGEMENTS
  ;; =========================================================================

  ;; Alternate values across cycles:
  (play!
   :alt-drums (s [:kick (alt :snare :clap)
                  :kick (alt :snare [:snare :snare])]))

  ;; Concatenate cycles over time with slowcat:
  (play!
   :progression
   (slowcat
    ;; Cycle 1: sparse intro
    (s [:kick :- :- :-])
    ;; Cycle 2: full groove
    (s [:kick :snare :kick :snare])
    ;; Cycle 3: break
    (s [:- :clap :- [:snare :snare]])))

  ;; Reverse the beat every 4th cycle:
  (play!
   :beat (-> (s [:kick (euclid 3 8 :hat) :snare :hat])
             (every-cycle 4 rev)))

  (stop!)

  ;; =========================================================================
  ;; 10. CONTINUOUS SIGNALS (LFOs) & REPEATABLE RANDOMNESS
  ;; =========================================================================

  ;; Smooth sweeping filter LFO with sine-sig:
  (play!
   :sweep (-> (note [:c3 :eb3 :g3 :bb3])
              (s :saw)
              (lpf (sine-sig 0.25 400 3500))
              (pan (sine-sig 0.5 -0.8 0.8))
              (gain 0.5)))

  ;; Repeatable randomness (seeded):
  (seed! 42)
  (play!
   :generative (-> (note (choose [:c4 :d4 :eb4 :g4 :ab4 :c5]))
                   (fast 2)
                   (s :sine)
                   (perc 0.005 0.2)
                   (pan (srand -0.7 0.7))
                   (room 0.5)
                   (gain 0.4)))

  (stop!)

  ;; =========================================================================
  ;; 11. SAMPLE LOADING & SLICING
  ;; =========================================================================

  ;; (load-sample! :sample-key "path/to/file.wav")
  ;; (load-freesound! :freesound-key 20933)
  ;; (slice-sample! :slice-name :source-key begin-norm end-norm)

  (comment
    (load-freesound! :amen 20933)
    (slice-sample! :amen-hit :amen 0.0 0.125)
    (slice-sample! :amen-snare :amen 0.25 0.375)
    (play!
     :amen-loop
     (s [:amen-hit :amen-snare [:amen-hit :amen-hit] :amen-snare])))

  ;; =========================================================================
  ;; 12. HARDWARE MIDI & CONTROLLER LIGHTS
  ;; =========================================================================

  (comment
    ;; 1. Connect to your MIDI device
    (midi-in-devices)
    (midi-in-connect!)

    ;; 2. Enable logging to discover hardware CC numbers
    (midi-debug! true)
    (midi-debug! false)

    ;; 3. Map a knob to filter cutoff
    (def-midi-cc! :knob-cutoff 74 :min 200 :max 8000 :curve :exp :default 800)

    ;; 4. Play with live knob tweaking
    (play!
     :midi-acid (-> (note [:c2 :eb2 :g2 :c3])
                    (s :tb303)
                    (lpf (midi-cc :knob-cutoff))))

    ;; 5. Performance pad toggle
    (def-midi-pad-toggle! 36
      (fn [_] (play! :kick (s (euclid 4 8 :kick))))
      (fn [_] (stop! :kick)))

    ;; 6. Hardware Launchpad/SmartPAD grid lighting
    (midi-out-connect!)
    (play!
     :light-show (-> (light-grid [:red :blue :green :yellow])
                     (fast 2))))

  ;; =========================================================================
  ;; 13. PUTTING IT ALL TOGETHER: MINI-TRACK
  ;; =========================================================================

  ;; Run this complete layered arrangement!
  (play-only!
   :kick (-> (s (euclid 4 8 :dub-kick))
             (gain 0.9)
             (duck-trigger 1))

   :snare (-> (s [:- :snare :- (alt :snare [:snare :snare])])
              (gain 0.8)
              (room 0.2))

   :hats (-> (s (euclid 7 16 :hat))
             (gain 0.4)
             (pan (sine-sig 0.5 -0.6 0.6)))

   :bass (-> (note [:c2 :c2 :eb2 :g2 :f2 :-])
             (s :tb303)
             (mono)
             (glide 0.05)
             (lpf 700)
             (resonance 0.8)
             (duck 0.75)
             (gain 0.8))

   :lead (-> (note [:c4 (-> :eb4 (glide 0.1)) :g4 (-> :bb4 (gain 0.9))])
             (s :saw)
             (lpf (sine-sig 0.25 600 3500))
             (space 0.4 0.25 4)
             (gain 0.4)))

  ;; When finished:
  (stop!))
