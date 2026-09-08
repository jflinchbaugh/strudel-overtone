(ns strudel-overtone.song-acid-techno
  "Showcase Song: Peak-Time Acid Techno
   Demonstrates:
   - Driving 138 BPM industrial techno groove with 4-on-the-floor kick
   - Euclidean rhythm generators (`euclid`) for syncopated percussion & claps
   - Screaming monophonic 303 acid lines with fast portamento (`glide`), high resonance, and filter sweeps
   - Inharmonic additive metallic percussion (`def-additive!`)
   - Sidechain compression pumping (`duck`, `duck-trigger`)
   - Dynamic live-coding arrangement sections"
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

;; Custom metallic/clang additive synth for industrial percussion
(def-additive! :acid-metal [1.0 0.85 0.55 0.35 0.2] :step 2.618)

(comment
  ;; ===========================================================================
  ;; PEAK-TIME ACID TECHNO SHOWCASE (138 BPM = (cpm 138/4))
  ;; ===========================================================================

  (cpm 138/4)

  ;; ---------------------------------------------------------------------------
  ;; SECTION A: THE ACID FOUNDATION (Kick + Driving Offbeat Hat + 303 Bass)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; Heavy 4/4 Techno Kick
   :kick (-> (s [:dance-kick :dance-kick :dance-kick :dance-kick])
             (gain 1.0)
             (duck-trigger 1.0)
             (duck-release 0.22))

   ;; Driving offbeat open/closed hats using Euclidean distribution
   :hats (-> (s (euclid 7 16 :hat))
             (gain 0.45)
             (pan (sine-sig 1 -0.5 0.5))
             (duck 0.55))

   ;; 16th-note sub rumble bassline
   :rumble (-> (note [:c1 :c1 :c1 :c1])
               (s :sine)
               (fast 2)
               (lpf 160)
               (gain 0.85)
               (distort 0.3)
               (duck 0.95))

   ;; Monophonic 303 Acid line with tight glide and filter envelope
   :acid (-> (note [:c2 [:c2 :c3] :eb2 [:f2 :g2] :c2 [:bb1 :c2] :eb2 :g2])
             (s :tb303)
             (mono)
             (glide 0.04)
             (lpf 400)
             (lpf-env 3800)
             (lpf-adsr 0.01 0.12 0.1 0.1)
             (adsr 0.01 0.18 0.6 0.1)
             (resonance 0.8)
             (distort 0.35)
             (gain 0.75)
             (duck 0.8)))

  ;; ---------------------------------------------------------------------------
  ;; SECTION B: FULL PEAK-TIME MADNESS (Claps, Metal Perk, Dual Resonant 303)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; 4/4 Kick with 4th-bar roll fill
   :kick (-> (s [:dance-kick :dance-kick :dance-kick :dance-kick])
             (gain 1.0)
             (duck-trigger 1.0)
             (duck-release 0.22)
             (every-cycle 4 (fn [p] (fast p 2))))

   ;; Syncopated Euclidean claps with reverb
   :clap (-> (s (euclid 3 8 :clap :- 1))
             (gain 0.75)
             (room 0.35)
             (echo 0.125 3))

   ;; Slicing Euclidean hats
   :hats (-> (s (euclid 11 16 :hat))
             (gain 0.4)
             (pan (saw-sig 2 -0.6 0.6))
             (duck 0.6))

   ;; Inharmonic metallic industrial percussion accents
   :metal (-> (note (euclid 5 16 :c5 :- 2))
              (s :acid-metal)
              (perc 0.001 0.08)
              (gain 0.45)
              (pan (srand -0.7 0.7))
              (echo 0.18 4)
              (room 0.4))

   ;; Main screaming 303 Acid lead with dynamic filter modulation
   :acid (-> (note [:c2 [:c2 :c3] :eb2 [:f2 :g2] :c2 [:bb2 :g2] :eb2 [:f2 :gb2]])
             (s :tb303)
             (mono)
             (glide 0.05)
             (lpf 500)
             (lpf-env 5500)
             (lpf-adsr 0.01 0.14 0.15 0.1)
             (adsr 0.01 0.2 0.7 0.1)
             (resonance 0.88)
             (res-env 0.2)
             (distort 0.4)
             (gain 0.8)
             (duck 0.85)
             (every-cycle 4 2 (fn [p] (add p 12))))

   ;; Sub rumble
   :rumble (-> (note [:c1 :c1 :c1 :c1])
               (s :sine)
               (fast 2)
               (lpf 160)
               (gain 0.85)
               (distort 0.3)
               (duck 0.95)))

  ;; ---------------------------------------------------------------------------
  ;; SECTION C: ACID HYPNOSIS BREAKDOWN (No Kick, Expanding Echo & Reverb)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; Wide metallic clicks
   :metal (-> (note (euclid 3 8 :c6))
              (s :acid-metal)
              (perc 0.001 0.15)
              (gain 0.4)
              (echo 0.375 6)
              (room 0.7))

   ;; Evolving high-resonance 303 solo with LFO filter sweep
   :acid (-> (note [:c3 :eb3 :f3 :g3 :bb3 :c4 :bb3 :g3])
             (s :tb303)
             (mono)
             (glide 0.08)
             (lpf (sine-sig 0.15 400 4500))
             (resonance 0.85)
             (distort 0.35)
             (echo 0.25 5)
             (room 0.6)
             (gain 0.7))

   ;; Dark atmospheric background drone
   :drone (-> (note [#{:c2 :g2}])
              (s :mooger)
              (slow 4)
              (lpf (sine-sig 0.1 300 1500))
              (distort 0.2)
              (room 0.7)
              (gain 0.35)))

  ;; Stop playback
  (stop!)

  (ov/stop)
  )
