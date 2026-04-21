(ns strudel-overtone.showcase
  (:require [overtone.core :as ov]
            [strudel-overtone.core :refer :all]))

(stop!)

(comment
  ;; 1. Simple Beat with Vectors
  (play! :drums (-> (s [:kick [:snare :snare] :kick :snare])
                  (note [:c2])
                    (gain 0.8)
                    (duck-trigger 1)))

  ;; 2. Acid Bassline (Mono with Glide)
  (play! :bass (-> (note [:c2 [:c2 :eb2] :g2 :_])
                   (s :tb303)
                   (mono)
                   (glide 0.05)
                   (lpf (sine 4 500 2000))
                   (resonance 0.8)
                   (gain 0.6)
                   (duck 0.8)))

  ;; 3. Shimmering Plucks (Functional Randomness)
  (play! :plucks (-> (note (choose-n 8 (ov/chord :c5 :major7)))
                     (s :ks-stringer)
                     (fast 2)
                     (pan (srand -1 1))
                     (room 0.6)
                     (echo-delay 0.25)
                     (gain 0.6)))

  ;; 4. Atmospheric Pad (Overlay)
  (play! :pad (-> (note [#{:c3 :e3 :g3 :b3}])
                  (s :mooger)
                  (slow 4)
                  (attack 2)
                  (release 2)
                  (gain (overlay [0.2 0.4 0.2]))
                  (lpf 1200)))


  ;; 5. Sample Slicing (Freesound)
  ;; Requires internet and Freesound API key if applicable,
  ;; but let's assume a sample is loaded.
  (load-freesound! :break 202537) ;; Amen Break
  (slice-sample! :bd :break 0 0.05)
  (slice-sample! :sd :break 0.12 0.18)

  (play! :amen (-> (s [:bd [:bd :bd] :sd [:sd :_]])
                   (fast 1)
                   (gain 0.8)))

  ;; 6. Dynamic Tempo
  (cpm 120)
  (glide-cpm 20 8) ; Speed up to 160 CPM over 8 cycles

  ;; 7. Stop everything
  (stop!)

)
