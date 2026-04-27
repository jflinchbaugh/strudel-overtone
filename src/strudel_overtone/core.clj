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
             sine saw tri square cosine sig-range)

;; Pattern records & builders
(import-vars p
             ->Event ->Pattern ->Overlay make-pattern parse-mini
             with-param set-param params overlay)

;; DSL Modifiers
(import-vars p
             s simul note gain swing duck duck-trigger duck-attack
             duck-release lpf pan resonance attack decay s-level sustain
             legato monophonic mono release width carrier-ratio
             modulator-ratio mod-index detune add chaos coef crush
             distort hpf bpf room room-size damp vibrato echo-delay
             echo-repeats rate speed pshift fshift tremolo-hz
             tremolo-depth pan-hz pan-depth phaser-hz phaser-depth begin
             end looping env active fast slow early late ribbon rev
             sometimes degrade delay-cycles stop-after glide
             adsr perc fm echo)

;; --- Player Re-exports ---

(import-vars player
             metro cpm glide-cpm player-state at-metro gate-off
             update-mono-inst start-mono-inst at-metro-mono
             trigger-event trigger-single-event apply-swing cycle-n
             play-loop playing play! play-only! stop!)

;; --- Synths Re-exports ---
(defmacro def-additive! [name ratios]
  `(synths/def-additive! ~name ~ratios))

(import-vars synths
             get-synth-name resolve-synth
             kick-adsr kick-perc snare-adsr snare-perc hat-adsr hat-perc
             clap-adsr clap-perc saw-adsr saw-perc sine-adsr sine-perc
             square-adsr square-perc tri-adsr tri-perc fm-adsr fm-perc
             white-adsr white-perc pink-adsr pink-perc brown-adsr
             brown-perc gray-adsr gray-perc clip-adsr clip-perc
             crackle-adsr crackle-perc dust-adsr dust-perc dust2-adsr
             dust2-perc lf-noise0-adsr lf-noise0-perc lf-noise1-adsr
             lf-noise1-perc lf-noise2-adsr lf-noise2-perc tb303-adsr
             tb303-perc supersaw-adsr supersaw-perc mooger-adsr
             mooger-perc ks-stringer-adsr ks-stringer-perc
             dub-kick-adsr dub-kick-perc dance-kick-adsr
             dance-kick-perc sampler-adsr sampler-perc)

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
          (pan (sine 0.1 -1 1))))

  (stop! :sd)

  ;; Change tempo
  (cpm 140)
  (glide-cpm 80 4)

  ;; Monophonic example
  (play! :mono (-> (note [:c3 :e3 :g3 :b3]) (s :saw) (mono) (glide 0.1) (fast 2)))

  (stop!)
)
