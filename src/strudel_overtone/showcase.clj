(ns strudel-overtone.showcase
  (:require [overtone.core :as ov]
            [strudel-overtone.strudel-overtone :refer :all]))

;; --- Initialization ---
;; (boot-server) ; If not already running

(comment
  ;; 1. Simple Beat with Vectors
  (play! :drums (-> (s [:kick [:snare :snare] :kick :snare])
                    (gain 0.8)
                    (duck-trigger 1)))

  ;; 2. Acid Bassline (Mono with Glide)
  (play! :bass (-> (note [:c2 [:c2 :eb2] :g2 :_])
                   (s :tb303)
                   (mono)
                   (glide 0.05)
                   (lpf (sine 0.5 500 2000))
                   (resonance 0.8)
                   (duck 0.8)))

  ;; 3. Shimmering Plucks (Functional Randomness)
  (play! :plucks (-> (note (choose-n 8 (ov/chord :c5 :major7)))
                     (s :ks-stringer)
                     (fast 4)
                     (pan (srand -1 1))
                     (room 0.6)
                     (echo-delay 0.25)
                     (gain 0.4)))

  ;; 4. Atmospheric Pad (Overlay & Jux)
  (play! :pad (-> (note [#{:c3 :e3 :g3 :b3}])
                  (s :mooger)
                  (slow 4)
                  (attack 2)
                  (release 2)
                  (gain (overlay [0.3 0.5 0.4]))
                  (jux rev)
                  (lpf 1200)))

  ;; 5. Sample Slicing (Freesound)
  ;; Requires internet and Freesound API key if applicable, 
  ;; but let's assume a sample is loaded.
  (load-freesound! :break 20933) ;; Amen Break
  (slice-sample! :bd :break 0 0.05)
  (slice-sample! :sd :break 0.12 0.18)

  (play! :amen (-> (s [:bd [:bd :bd] :sd [:sd :_]])
                   (fast 2)
                   (gain 0.8)))

  ;; 6. Dynamic Tempo
  (cpm 120)
  (glide-cpm 160 8) ; Speed up to 160 CPM over 8 cycles

  ;; 7. Stop everything
  (stop!)
)
