(ns strudel-overtone.rhythm-showcase
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

;; Showcase: Euclidean Rhythms & Periodic Cycle Modifiers
;; Demonstrates euclid (Bjorklund rhythm distribution) and every-cycle.

(comment
  (stop!)

  ;; --- 1. Classic Euclidean Beats ---
  ;; Tresillo rhythm: 3 hits across 8 steps -> [:kick :- :- :kick :- :- :kick :-]
  (play! :kick (s (euclid 3 8 :dance-kick)))

  ;; Cinquillo rhythm: 5 hits across 8 steps
  (play! :hat (-> (s (euclid 5 8 :hat))
                  (gain 0.7)))

  ;; Fandango/Clap: 4 hits across 12 steps
  (play! :clap (-> (s (euclid 4 12 :clap))
                   (gain 0.8)
                   (room 0.3)))

  (stop!)

  ;; --- 2. Euclidean Polyrhythms ---
  ;; Layering distinct Euclidean step counts for interlocking syncopation
  (play-only!
   :kick (s (euclid 3 8 :dub-kick))
   :snare (s (euclid 2 5 :snare :- 1)) ; Khafif-e-ramal rotated by 1
   :hat (-> (s (euclid 7 16 :hat))
            (gain 0.6)
            (pan (sine-sig 0.25 -0.6 0.6))))

  (stop!)

  ;; --- 3. Melodic Euclidean Sequences ---
  ;; Using euclid directly with keyword pitch values and rotation
  (play-only!
   :lead (-> (note (euclid 5 8 :c3 :- 2))
             (s :saw)
             (lpf 900)
             (lpf-env 2500)
             (lpf-adsr 0.01 0.15 0.1 0.2)
             (adsr 0.01 0.2 0.5 0.1)
             (echo 0.2 4)))

  (stop!)

  ;; --- 4. Periodic Variations (every-cycle) ---
  ;; Reverses the drum loop every 4th cycle
  (play! :drums
         (-> (s [:kick (euclid 3 8 :hat) (alt :snare :clap) :hat])
             (every-cycle 4 rev)))

  ;; Accelerates / doubles tempo every 8th cycle
  (play! :fast-fill
         (-> (s [:kick :snare :kick :clap])
             (every-cycle 8 (fn [p] (fast p 2)))))

  (stop!)

  ;; --- 5. Full Euclidean Groove with Every-Cycle Fills ---
  (cpm 30)

  (play-only!
   ;; Kick with 3/8 Tresillo, doubled every 4th cycle
   :kick (-> (s (euclid 3 8 :dub-kick))
             (gain 1.0)
             (duck-trigger 1)
             (every-cycle 4 (fn [p] (fast p 2))))

   ;; Snare on 2 and 4, alternating clap every other cycle
   :snare (-> (s [:- (alt :snare :clap) :- :snare])
              (gain 0.8)
              (room 0.2))

   ;; 7/16 Euclidean hat with dynamic LFO panning
   :hat (-> (s (euclid 7 16 :hat))
            (gain 0.6)
            (pan (saw-sig 0.5 -0.7 0.7))
            (every-cycle 4 rev))

   ;; Acid bassline using 5/8 Euclidean rhythm with filter sweeps
   :acid (-> (note (euclid 5 8 :c2 :- 1))
             (s :tb303)
             (lpf 400)
             (lpf-env 4000)
             (lpf-adsr 0.01 0.18 0.1 0.15)
             (adsr 0.01 0.2 0.6 0.1)
             (resonance 0.15)
             (duck 0.7)
             (every-cycle 8 4 (fn [p] (add p 12)))))

  (play-only!
   :lead (-> (s :sine)
             (note [:c4])
             (degrees :major [1 2 3 4])
             (every-cycle 2 (fn [p] (crush p 0.5)))
             (every-cycle 2 1 (fn [p] (distort p 0.5)))))

  (stop!)
)
