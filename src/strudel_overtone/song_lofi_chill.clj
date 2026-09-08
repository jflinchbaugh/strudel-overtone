(ns strudel-overtone.song-lofi-chill
  "Showcase Song: Lo-Fi Hip-Hop / Chillhop Study Beats
   Demonstrates:
   - Relaxed 76 BPM tempo with authentic swing / shuffle groove (`swing`)
   - Vinyl crackle and warm noise bed textures (`:crackle`, `:pink`)
   - Custom dusty electric piano (`def-additive!`) with tremolo and subtle detune wobble
   - Lazy boom-bap drum pattern with sidechain pumping (`duck-trigger`, `duck`)
   - Warm round sub-bass (`:sine`) and tape-delayed melodic plucks (`:ks-stringer`, `echo`)"
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

;; Custom warm additive electric piano synth
(def-additive! :lofi-epiano [1.0 0.5 0.22 0.12 0.05])

(comment
  ;; ===========================================================================
  ;; LO-FI CHILLHOP / STUDY BEATS SHOWCASE (76 BPM = (cpm 19))
  ;; ===========================================================================

  (cpm 19)

  ;; ---------------------------------------------------------------------------
  ;; SECTION A: THE CHILL GROOVE (Full Warm Beat)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; Vinyl crackle texture
   :crackle (-> (s :crackle)
                (gain 0.12)
                (lpf 3200)
                (room 0.3))

   ;; Lazy boom-bap kick with sidechain compression trigger
   :kick (-> (s [:dub-kick :- [:- :dub-kick] :-])
             (gain 0.9)
             (duck-trigger 1.0)
             (duck-release 0.35)
             (swing 1/16))

   ;; Warm snappy snare with gentle room reverb
   :snare (-> (s [:- :snare :- :snare])
              (gain [0.75 0.85])
              (room 0.3)
              (lpf 4000)
              (swing 1/16))

   ;; Swung, humanized hi-hats with velocity overlay
   :hats (-> (s [:hh :hh :hh :hh :hh :hh :hh :hh])
             (gain (overlay [0.35 0.18 0.28 0.15 0.32 0.18 0.25 0.2]))
             (pan (sine 0.5 -0.3 0.3))
             (lpf 6000)
             (swing 1/16)
             (duck 0.45))

   ;; Warm, round sub-bassline with gentle portamento
   :bass (-> (note [:eb2 :_ [:eb2 :g2] :_ :ab2 :_ [:bb2 :c3] :_])
             (s :sine)
             (mono)
             (glide 0.08)
             (lpf 320)
             (distort 0.15)
             (gain 0.85)
             (duck 0.9))

   ;; Dusty, warm 7th/9th electric piano chords with vintage tremolo
   :epiano (-> (note [#{:eb3 :g3 :bb3 :d4}
                      #{:g3 :bb3 :d4 :f4}
                      #{:ab3 :c4 :eb4 :g4}
                      #{:bb3 :d4 :f4 :ab4}])
               (s :lofi-epiano)
               (slow 2)
               (tremolo-hz 3.5)
               (tremolo-depth 0.3)
               (adsr 0.06 0.5 0.5 0.4)
               (lpf 2200)
               (room 0.4)
               (gain 0.4)
               (duck 0.7))

   ;; Nostalgic tape-delayed pluck melody
   :lead (-> (note [:- :g4 :bb4 [:d5 :c5] :- :bb4 :g4 [:f4 :eb4]])
             (s :ks-stringer)
             (gain 0.35)
             (echo 0.375 4)
             (crush 0.12)
             (room 0.55)
             (pan 0.2)))

  ;; ---------------------------------------------------------------------------
  ;; SECTION B: MIDNIGHT INTERLUDE (Stripped Back Atmospheric Chords & Solo)
  ;; ---------------------------------------------------------------------------
  (play-only!
   :crackle (-> (s :crackle)
                (gain 0.14)
                (lpf 2800))

   ;; Soft background kick pulse
   :kick (-> (s [:dub-kick :_ :_ :_])
             (gain 0.6)
             (lpf 200))

   ;; Deep warm sub
   :bass (-> (note [:eb1 :_ :g1 :_ :ab1 :_ :bb1 :_])
             (s :sine)
             (mono)
             (glide 0.1)
             (gain 0.7)
             (lpf 250))

   ;; Dreamy sweeping E-piano chords
   :epiano (-> (note [#{:eb3 :g3 :bb3 :d4}
                      #{:g3 :bb3 :d4 :f4}
                      #{:ab3 :c4 :eb4 :g4}
                      #{:bb3 :d4 :f4 :ab4}])
               (s :lofi-epiano)
               (slow 2)
               (lpf (sine 0.15 800 2400))
               (room 0.6)
               (gain 0.45))

   ;; Expressive solo with vibrato and tape echo
   :lead (-> (note [:d4 [:eb4 :f4] :g4 [:bb4 :c5] :d5 [:c5 :bb4] :g4 :eb4])
             (s :mooger)
             (mono)
             (glide 0.09)
             (vibrato 3)
             (lpf 2400)
             (echo 0.375 5)
             (room 0.6)
             (gain 0.38)))

  ;; Stop all tracks
  (stop!)
  (ov/stop)
  )
