(ns strudel-overtone.additive-showcase
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

;; --- Additive Synth Definitions ---

;; 1. Classic Organ: Standard harmonic series (step 1)
(def-additive! :add-organ [1.0 0.6 0.4 0.3 0.2 0.1])

;; 2. Hollow Wood: Odd harmonics only (step 2)
(def-additive! :add-hollow [1.0 0.0 0.7 0.0 0.4] :step 2)

;; 3. Metallic Bell: Inharmonic partials (step 1.618 - Golden Ratio)
(def-additive! :add-bell [1.0 0.8 0.6 0.4 0.2] :step 1.618)

;; 4. Industrial Drone: Wide spacing (step 4.5)
(def-additive! :add-industrial [1.0 0.9 0.8] :step 4.5)

(comment
  (stop!)

  ;; 1. Bassline using the Organ synth
  (play! :bass
         (-> (note [:c2 :c2 :f1 :g1])
             (s :add-organ)
             (lpf 400)
             (attack 0.05)
             (sustain 0.2)
             (gain 0.0)))

  ;; 2. Hollow melody (Woody/Clarinet-like)
  (play! :melody
         (-> (note [[:c4 :d4] :eb4 [:f4 :g4] :bb4])
             (s :add-hollow)
             (attack 0.01)
             (sustain 0.1)
             (detune -50)
             (echo 0.25 5)
             (room 0.3)))

  ;; 3. Occasional Bell accents
  (play! :accents
         (-> (note [:- :c5 :- :g5])
             (s :add-bell)
             (perc  0.001 0.5)
             (detune 10)
             (room 0.6)
             (gain 0.6)))

  ;; 4. Industrial textural layer
  (play! :texture
         (-> (note [:c1 :c1 :c1 :c1])
             (s :add-industrial)
             (mono)
             (vibrato 2)
             (detune (irand -100 100))
             (lpf 300)
             (gain 0.3)
             (slow 1)))


  (run)

  (stop!)

  (ov/stop)

  )
