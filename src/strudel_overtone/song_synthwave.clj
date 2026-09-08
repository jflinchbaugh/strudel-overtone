(ns strudel-overtone.song-synthwave
  "Showcase Song: 80s Synthwave / Outrun Cyberpunk
   Demonstrates:
   - Driving 4-on-the-floor beat with sidechain ducking (`duck-trigger`, `duck`)
   - 16th-note rolling bassline with filter envelope modulation (`lpf-adsr`, `lpf-env`)
   - Lush polyphonic supersaw pad chords with panning LFO
   - Plucked arpeggio lead with stereo ping-pong echo (`echo`, `pan`)
   - Dynamic section arrangement using `play-only!` blocks"
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

(comment
  ;; ===========================================================================
  ;; 80s SYNTHWAVE / RETROWAVE SHOWCASE (120 BPM = (cpm 30))
  ;; ===========================================================================

  ;; 1. Set tempo
  (cpm 30)

  ;; ---------------------------------------------------------------------------
  ;; SECTION A: INTRO ATMOSPHERE (Ambient Pad + Chime Pluck)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; Shimmering intro arpeggios
   :arp (-> (note [:d4 :f4 :a4 :c5 :d5 :c5 :a4 :f4])
            (s :ks-stringer)
            (fast 2)
            (gain 0.45)
            (room 0.6)
            (echo 0.25 4)
            (pan (sine 0.5 -0.6 0.6)))

   ;; Warm vintage chords
   :pad (-> (note [#{:d3 :f3 :a3 :c4}
                   #{:bb2 :d3 :f3 :a3}
                   #{:c3 :e3 :g3 :b3}
                   #{:a2 :c3 :e3 :g3}])
            (s :supersaw)
            (slow 2)
            (lpf 900)
            (lpf-env 1800)
            (lpf-adsr 0.8 1.2 0.6 1.0)
            (adsr 0.6 0.3 0.7 0.8)
            (room 0.5)
            (gain 0.3)))

  ;; ---------------------------------------------------------------------------
  ;; SECTION B: THE MAIN GROOVE (Full 80s Outrun Drive)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; Punchy 4-on-the-floor kick acting as the sidechain pump master
   :kick (-> (s [:dance-kick :dance-kick :dance-kick :dance-kick])
             (gain 0.95)
             (duck-trigger 1.0)
             (duck-release 0.28))

   ;; Gated & reverbed 80s snare + layered clap on beats 2 & 4
   :snare (-> (s [:- [:snare :clap] :- [:snare :clap]])
              (gain 0.8)
              (room 0.45)
              (echo 0.05 2))

   ;; Driving 16th hi-hats with accent alternation
   :hats (-> (s [:hh :hh :hh :hh :hh :hh :hh :hh])
             (fast 2)
             (gain (overlay [0.25 0.15 0.2 0.15]))
             (pan (sine 1 -0.4 0.4))
             (duck 0.6))

   ;; 16th rolling synthwave bassline with filter sweeps and sidechain pumping
   :bass (-> (note [:d2 :d2 :d2 :d2 :bb1 :bb1 :bb1 :bb1 :c2 :c2 :c2 :c2 :a1 :a1 :a1 :a1])
             (s :saw)
             (fast 2)
             (lpf 500)
             (lpf-env 2400)
             (lpf-adsr 0.01 0.16 0.1 0.08)
             (adsr 0.01 0.2 0.7 0.05)
             (distort 0.25)
             (gain 0.75)
             (duck 0.9))

   ;; Vintage analog pad chords sidechained to the kick
   :pad (-> (note [#{:d3 :f3 :a3 :c4}
                   #{:bb2 :d3 :f3 :a3}
                   #{:c3 :e3 :g3 :b3}
                   #{:a2 :c3 :e3 :g3}])
            (s :supersaw)
            (slow 2)
            (lpf 1100)
            (adsr 0.4 0.2 0.8 0.6)
            (gain 0.35)
            (room 0.5)
            (duck 0.85))

   ;; Neon lead melody with stereo delay
   :lead (-> (note [[:d4 :f4] :a4 [:d5 :c5] :a4
                    [:bb4 :a4] :g4 [:f4 :e4] :d4])
             (s :mooger)
             (mono)
             (glide 0.05)
             (lpf 2800)
             (resonance 0.3)
             (echo 0.25 5)
             (room 0.4)
             (gain 0.45)
             (duck 0.5)))

  ;; ---------------------------------------------------------------------------
  ;; SECTION C: FILTERED BREAKDOWN (No Drums, Sweeping Filters)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; Filtered bass pulse
   :bass (-> (note [:d2 :_ :bb1 :_ :c2 :_ :a1 :_])
             (s :tb303)
             (mono)
             (glide 0.12)
             (lpf (sine 0.25 300 1200))
             (resonance 0.6)
             (gain 0.6))

   ;; Swelling pad
   :pad (-> (note [#{:d3 :f3 :a3 :c4}
                   #{:bb2 :d3 :f3 :a3}
                   #{:c3 :e3 :g3 :b3}
                   #{:a2 :c3 :e3 :g3}])
            (s :supersaw)
            (slow 2)
            (lpf (sine 0.1 600 3500))
            (room 0.7)
            (gain 0.4))

   ;; Delay arp
   :arp (-> (note [:d4 :f4 :a4 :c5 :d5 :c5 :a4 :f4])
            (s :ks-stringer)
            (fast 2)
            (gain 0.4)
            (echo 0.25 6)
            (room 0.6)))

  ;; ---------------------------------------------------------------------------
  ;; SECTION D: PEAK DROP / CLIMAX (Fast fills & doubled lead)
  ;; ---------------------------------------------------------------------------
  (play-only!
   :kick (-> (s [:dance-kick :dance-kick :dance-kick [:dance-kick :dance-kick]])
             (gain 1.0)
             (duck-trigger 1.0)
             (duck-release 0.25))

   :snare (-> (s [:- [:snare :clap] :- [:snare [:snare :snare]]])
              (gain 0.85)
              (room 0.4)
              (every-cycle 4 rev))

   :hats (-> (s [:hh :hh :hh :hh :hh :hh :hh :hh])
             (fast 2)
             (gain (overlay [0.3 0.18 0.24 0.18]))
             (duck 0.7))

   :bass (-> (note [:d2 :d2 :d2 :d2 :bb1 :bb1 :bb1 :bb1 :c2 :c2 :c2 :c2 :a1 :a1 :a1 :a1])
             (s :saw)
             (fast 2)
             (lpf 750)
             (lpf-env 3500)
             (lpf-adsr 0.01 0.14 0.1 0.06)
             (distort 0.35)
             (gain 0.8)
             (duck 0.9))

   :lead (-> (note [[:d4 :f4] :a4 [:d5 :c5] :a4
                    [:bb4 :a4] :g4 [:f4 :e4] :d4])
             (s :supersaw)
             (lpf 4000)
             (echo 0.25 5)
             (room 0.5)
             (gain 0.5)
             (duck 0.6)))

  ;; Stop all tracks
  (stop!)
  (ov/stop)
  )
