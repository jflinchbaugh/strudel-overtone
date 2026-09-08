(ns strudel-overtone.inline-showcase
  "Showcase of inline note/token parameter modifications.
   Demonstrates how to attach expressive modifiers (gain, glide,
   lpf, adsr, with, detune, etc.) directly to individual notes,
   sub-sequence vectors, and chords/sets."
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

(comment
  (stop!)

  ;; --- 1. Expressive Melody with Per-Note Dynamics and Glides ---
  ;; Notes can individually carry dynamics (gain), pitch slides (glide),
  ;; or custom filters without affecting the surrounding sequence.
  (play! :expressive-lead
         (-> (note [:c4
                    (-> :eb4 (gain 0.4) (glide 0.1))
                    (-> :g4 (gain 0.9) (lpf 3000))
                    (-> :bb4 (glide 0.2))])
             (s :saw)
             (mono)
             (room 0.3)
             (space 0.4 0.25 4)))

  (stop!)

  ;; --- 2. Vector Sub-sequences with Grouped Modifiers ---
  ;; Applying a modification to a vector applies it across all items
  ;; in that sub-sequence.
  (play! :vector-groove
         (-> (note [:c3
                    (-> [:eb3 :g3 :bb3] (gain 0.5) (lpf 900))
                    :c4
                    (-> [:d4 :c4] (gain 0.8) (glide 0.08))])
             (s :tb303)
             (mono)
             (gain 0.7)))

  (stop!)

  ;; --- 3. Dynamic Chords (Sets) & Voicings ---
  ;; Chords / sets can be accented or filtered as a unit inline.
  (play! :chord-prog
         (-> (note [#{:c3 :g3 :c4}
                    (-> #{:eb3 :bb3 :eb4} (gain 0.5) (lpf 1200))
                    (-> #{:f3 :c4 :f4} (gain 0.8) (lpf 2500))
                    #{:g3 :d4 :g4}])
             (s :mooger)
             (slow 2)
             (adsr 0.05 0.3 0.6 0.4)
             (space 0.5 0.35 4)))

  (stop!)

  ;; --- 4. Using the `with` Helper for Multi-Param Decoration ---
  ;; `with` accepts maps or key-value pairs for concise parameter packs.
  (play! :with-demo
         (-> (note [:c3
                    (with :eb3 {:gain 0.5 :lpf 800 :glide 0.1})
                    :g3
                    (with [:bb3 :c4] :gain 0.9 :lpf 3500)])
             (s :saw)
             (mono)
             (gain 0.6)))

  (stop!)

  ;; --- 5. Full Arrangement: "Neon Micro-Dynamics" ---
  (cpm 120)

  (play-only!
   ;; Drums with ghost note accents inline
   :drums (s [:dance-kick
              (-> [:hat (-> :hat (gain 0.3))] (pan -0.3))
              [:snare (-> :snare (gain 0.4))]
              (-> [:hat :hat :hat] (gain 0.6) (pan 0.3))])

   ;; Acid bass with slide accents on select notes
   :bass (-> (note [:c2
                    (-> :c2 (gain 0.5))
                    (-> [:eb2 :f2] (glide 0.1) (lpf 2200))
                    (-> :g2 (glide 0.15) (gain 0.9))])
             (s :tb303)
             (mono)
             (acid 800 0.08 3500)
             (gain 0.7)
             (duck 0.8))

   ;; Ambient chords with highlighted colorations
   :pad (-> (note [(-> #{:c3 :g3 :eb4} (gain 0.4) (lpf 900))
                   (-> #{:bb2 :f3 :d4} (gain 0.7) (lpf 1800))
                   (-> #{:ab2 :eb3 :c4} (gain 0.5) (lpf 1200))
                   (-> #{:g2 :d3 :b3} (gain 0.8) (lpf 2400))])
            (s :mooger)
            (slow 2)
            (adsr 0.2 0.5 0.7 0.8)
            (space 0.6 0.3 5)))

  (stop!)


  (reload!)

  )
