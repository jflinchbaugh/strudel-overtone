(ns strudel-overtone.song-liquid-dnb
  "Showcase Song: Liquid Drum & Bass / Atmospheric Jungle
   Demonstrates:
   - High tempo (174 BPM) syncopated drum breaks with ghost notes and swing
   - Deep rolling Reese & Sub-bass with portamento (`mono`, `glide`) and sidechain compression
   - Custom additive Rhodes electric piano (`def-additive!`) with tremolo modulation
   - Atmospheric Karplus-Strong string arpeggios (`:ks-stringer`)
   - Periodic drum variations and fills using `every-cycle`"
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

;; Define a lush additive Rhodes/E-Piano synth voice
(def-additive! :liquid-rhodes [1.0 0.65 0.35 0.18 0.08 0.04])

(comment
  ;; ===========================================================================
  ;; LIQUID DRUM & BASS SHOWCASE (174 BPM = (cpm 174/4))
  ;; ===========================================================================

  (cpm 174/4)

  ;; ---------------------------------------------------------------------------
  ;; SECTION A: LIQUID ROLLER (Full Atmospheric Groove)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; Fast syncopated D&B kick pattern with sidechain trigger
   :kick (-> (s [[:dub-kick :-] :- [:- :dub-kick] :-])
             (gain 0.95)
             (duck-trigger 1.0)
             (duck-release 0.18))

   ;; Syncopated snare with ghost hits and subtle room reverb
   :snare (-> (s [:- :snare [:- :snare] :snare])
              (gain [0.3 0.85 0.4 0.9])
              (room 0.25)
              (swing [1/32]))

   ;; Shuffling 16th hats with stereo panning and periodic roll
   :hats (-> (s [:hh :hh [:hh :hh] :hh :hh [:hh :hh] :hh :hh])
             (gain (overlay [0.3 0.15 0.25 0.15 0.35 0.15 0.25 0.2]))
             (pan (saw 0.5 -0.5 0.5))
             (duck 0.5)
             (every-cycle 4 (fn [p] (fast p 2))))

   ;; Deep rolling Reese sub-bass with portamento glide & filter movement
   :reese (-> (note [:f1 :_ [:f1 :ab1] :_ :eb1 :_ [:eb1 :bb1] :_])
              (s :tb303)
              (mono)
              (glide 0.08)
              (lpf (sine 0.5 280 1100))
              (resonance 0.35)
              (distort 0.2)
              (gain 0.85)
              (duck 0.95))

   ;; Soulful additive electric piano chords
   :rhodes (-> (note [#{:f3 :ab3 :c4 :eb4}
                      #{:db3 :f3 :ab3 :c4}
                      #{:eb3 :g3 :bb3 :d4}
                      #{:c3 :eb3 :g3 :bb3}])
               (s :liquid-rhodes)
               (slow 2)
               (tremolo-hz 4)
               (tremolo-depth 0.35)
               (adsr 0.08 0.4 0.6 0.5)
               (lpf 2600)
               (room 0.45)
               (gain 0.4)
               (duck 0.7))

   ;; Shimmering atmospheric stringer top arpeggios
   :sparkle (-> (note [:f4 :ab4 :c5 :eb5 :g5 :eb5 :c5 :ab4])
                (s :ks-stringer)
                (fast 2)
                (gain 0.35)
                (echo 0.17 4)
                (room 0.65)
                (pan (sine 0.25 -0.7 0.7))))

  ;; ---------------------------------------------------------------------------
  ;; SECTION B: ATMOSPHERIC CHILL BREAKDOWN (Deep Pad & Sparse Beats)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; Light ticking hats
   :hats (-> (s [:hh :_ :hh :_ :hh :_ [:hh :hh] :_])
             (gain 0.25)
             (pan (sine 1 -0.6 0.6)))

   ;; Deep sub pulse
   :sub (-> (note [:f1 :_ :db1 :_ :eb1 :_ :c1 :_])
            (s :sine)
            (mono)
            (glide 0.1)
            (gain 0.7)
            (lpf 350))

   ;; Sweeping filter Rhodes chords
   :rhodes (-> (note [#{:f3 :ab3 :c4 :eb4}
                      #{:db3 :f3 :ab3 :c4}
                      #{:eb3 :g3 :bb3 :d4}
                      #{:c3 :eb3 :g3 :bb3}])
               (s :liquid-rhodes)
               (slow 2)
               (lpf (sine 0.2 400 2800))
               (room 0.6)
               (gain 0.45))

   ;; Plucked melody with echoing delay
   :lead (-> (note [:c5 :eb5 :f5 :g5 :bb5 :g5 :f5 :eb5])
             (s :ks-stringer)
             (gain 0.38)
             (echo 0.25 6)
             (room 0.75)))

  ;; ---------------------------------------------------------------------------
  ;; SECTION C: INTENSE JUNGLE DROP (High Energy Amen-style Chops)
  ;; ---------------------------------------------------------------------------
  (play-only!
   ;; High energy syncopated break
   :drums (-> (s [[:dub-kick :dub-kick] :snare
                  [:- [:dub-kick :dub-kick]] [:snare [:snare :clap]]])
              (gain 0.95)
              (duck-trigger 1.0)
              (duck-release 0.15)
              (every-cycle 4 rev))

   :hats (-> (s [:hh :hh :hh [:hh :hh] :hh :hh [:hh :hh :hh] :hh])
             (gain 0.3)
             (pan (saw 1 -0.6 0.6)))

   ;; Aggressive distorted Reese bass
   :reese (-> (note [:f1 [:f1 :f2] :ab1 [:g1 :eb1] :db1 [:db1 :db2] :c1 [:eb1 :ab1]])
              (s :tb303)
              (mono)
              (glide 0.05)
              (lpf 1600)
              (resonance 0.5)
              (distort 0.45)
              (gain 0.8)
              (duck 0.9))

   ;; Ambient vocal-like synth layer
   :pad (-> (note [#{:f4 :ab4 :c5}])
            (s :supersaw)
            (slow 2)
            (lpf 1200)
            (room 0.55)
            (gain 0.3)
            (duck 0.85)))

  ;; Stop all playback
  (stop!)
  (ov/stop)
  )
