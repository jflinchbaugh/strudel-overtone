(ns strudel-overtone.core
  (:require [overtone.core :as ov]
            [taoensso.telemere :as tel]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.synths :as synths]
            [strudel-overtone.samples :as samples]
            [strudel-overtone.player :as player])
  (:import [strudel_overtone.pattern Event Pattern Overlay]))

(defmacro import-vars
  "Imports multiple vars from a namespace or alias, preserving metadata (docstrings, arglists)."
  [ns-alias-or-sym & syms]
  (let [ns-obj (or (find-ns ns-alias-or-sym)
                   (get (ns-aliases *ns*) ns-alias-or-sym))]
    `(do
       ~@(for [s syms]
           (let [v (ns-resolve ns-obj s)
                 m (meta v)
                 clean-m (select-keys m [:doc :arglists])
                 orig-sym (symbol (str (ns-name ns-obj)) (str s))]
             `(do
                (def ~s ~orig-sym)
                (alter-meta! (var ~s) merge '~clean-m)))))))

;; --- Pattern Engine Re-exports ---

;; Randomness
(import-vars p
             seed! irand srand repeatable-rand choose wchoose choose-n rtake)

;; Signals
(import-vars p
             sine-sig saw-sig tri-sig square-sig cosine-sig sig-range adsr-sig)

;; Pattern records & builders
(import-vars p
             ->Event ->Pattern ->Overlay make-pattern parse-mini
             with-param set-param params overlay)

;; DSL Modifiers
(import-vars p
             s simul note gain swing duck duck-trigger duck-attack
             duck-release lpf pan resonance sustain
             legato monophonic mono width carrier-ratio
             modulator-ratio mod-index detune add chaos coef crush
             distort hpf bpf room room-size damp vibrato echo-delay
             echo-repeats rate speed pshift fshift tremolo-hz
             tremolo-depth pan-hz pan-depth phaser-hz phaser-depth begin
             end looping env active fast slow early late ribbon rev
             sometimes degrade delay-cycles stop-after glide
             adsr perc lpfe lpf-adsr lpf-perc fm echo step degrees
             alt slowcat stack fastcat)

;; --- Player Re-exports ---

(import-vars player
             metro cpm glide-cpm player-state at-metro gate-off
             update-mono-inst start-mono-inst at-metro-mono
             trigger-event trigger-single-event apply-swing cycle-n
             play-loop playing play! play-only! stop!)

;; --- Synths Re-exports ---
(defmacro def-additive! [name ratios & opts]
  `(synths/def-additive! ~name ~ratios ~@opts))

(import-vars synths
             get-synth-name resolve-synth
             kick snare hat clap saw sine square tri fm
             white pink brown gray clip crackle dust dust2
             lf-noise0 lf-noise1 lf-noise2 tb303 supersaw mooger
             ks-stringer dub-kick dance-kick sampler)

;; --- Pitch/Music Re-exports ---
(import-vars ov
             chord scale chord-degree invert-chord find-chord
             find-note-name degree->interval)

(def note->midi ov/note)
(alter-meta! (var note->midi) merge (select-keys (meta #'ov/note) [:doc :arglists]))

;; --- Sampling Re-exports ---

(import-vars samples
             samples sample-slices load-sample! load-freesound!
             slice-sample! slice-sample-ms! sample-info)

;; --- Main / Entry ---

(defn -main [& args]
  (ov/connect-server)
  (tel/log! :info {:studel-overtone :ready}))

(comment

  ;; Play a bassline
  (play! :bass (-> (note [:c2 :g2]) (s :saw) (gain 0.5)))

  (stop!)

  ;; Layer drums on top (aligned)
  (play! :sd
         (->
          (s [:sine])
          (note :a2)
          (fast 16)
          (gain 1.0)
          (pan (srand -1 1))))

  (play! :hh
         (->
          (s [:sine])
          (note :a4)
          (fast 32)
          (gain (overlay [0.2 0.5 0.3 0.8]))
          (pan (sine-sig 0.1 -1 1))))

  (stop! :sd)

  ;; Change tempo
  (cpm 140)
  (glide-cpm 80 4)

  ;; Acid 303 Bassline with Filter Envelope Sweep
  (play! :acid
         (-> (note [:c2 :c3 :c2 :eb2 :g2])
             (s :tb303)
             (lpf 300)                      ; Base cutoff frequency = 300 Hz
             (lpfe 5000)                    ; Envelope modulation depth = 5000 Hz
             (lpf-adsr 0.01 0.15 0.1 0.2)   ; Plucky filter sweep
             (adsr 0.01 0.2 0.8 0.1)        ; Volume envelope
             (resonance 0.1)))

  (stop!)
)
