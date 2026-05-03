(ns strudel-overtone.multi-cycle-showcase
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

;; Showcase: Multi-Cycle Patterns
;; This file demonstrates complex ways to use alt, slowcat,
;; stack, and fastcat to create evolving compositions.

(comment
  ;; --- 1. Basic Alternation (alt) ---
  ;; Swaps the snare and clap every other cycle
  (play! :drums (-> (s [:kick (alt :snare :clap) :kick :snare])
                    (gain 0.8)))

  (stop!)

  ;; --- 2. Sequential Patterns (slowcat) ---
  ;; Chains multiple 1-cycle patterns together
  (play! :song
         (slowcat
          ;; Cycle 0: Kick/Snare
          (s [:kick :snare :kick :snare])
          ;; Cycle 1: Fast Hats
          (s [:hh :hh :hh :hh :hh :hh :hh :hh])
          ;; Cycle 2: Deep Bass
          (-> (note [:c2 :c2 :eb2 :f2]) (s :saw) (lpf 400))))

  ;; --- 3. Nested Alternation ---
  ;; You can put 'alt' inside other structures for complex evolution
  (play! :evolve
         (-> (note [:c3 (alt :e3 :g3) :f3 (alt :g3 :b3)])
             (s :sine)
             (attack 0.05)
             (sustain 0.2)))

  (stop!)

  ;; --- 4. Rhythmic Layers (stack) ---
  ;; Combines different structures into one logical pattern
  (play! :layered
         (stack
          ;; Foundation
          (s [:kick :- :kick :-])
          ;; Alternating Percussion
          (s [:- (alt :snare :cp) :- (alt :cp :snare)])
          ;; Constant shimmer
          (-> (s [:hh :hh :hh :hh]) (gain 0.3))))

  ;; --- 5. Tempo Compression (fastcat) ---
  ;; Squeezes multiple patterns into a single cycle
  (play! :glitch
         (fastcat
          (s [:kick :snare])
          (s [:hh :hh :hh :hh])
          (s [:clap :clap])))

  ;; --- 6. The "A/B" Arrangement ---
  ;; Creating a 4-cycle structure using slowcat and stack
  (let [verse (-> (note [:c3 :eb3]) (s :saw) (lpf 800))
        chorus (-> (note [:f3 :ab3 :g3 :c4]) (s :tb303) (lpf 1200))]
    (play! :arrangment
           (slowcat verse verse verse chorus)))

  ;; --- 7. Complex Harmonic Evolution ---
  ;; Combining LFOs with 'alt' and 'slowcat'
  (play! :ambient
         (-> (note (alt [:c4 :e4 :g4] [:f4 :a4 :c5] [:g4 :b4 :d5]))
             (s :sine)
             (gain (sine 0.2 0.3 0.7)) ; Volume swells
             (pan (sine 0.5 -1 1))     ; Panning LFO
             (slow 2)                 ; Play slower
             (attack 0.5)
             (release 1.0)))

  (stop!)

)
