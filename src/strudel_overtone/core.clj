(ns strudel-overtone.core
  (:require [overtone.core :as ov]
            [taoensso.telemere :as tel]
            [strudel-overtone.util :refer [import-vars]]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.synths :as synths]
            [strudel-overtone.samples :as samples]
            [strudel-overtone.player :as player]
            [strudel-overtone.midi :as midi])
  (:import [strudel_overtone.pattern Event Pattern Overlay]))

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
             acid drive space lfo
             adsr perc lpf-env lpf-adsr lpf-perc hpf-env hpf-adsr
             hpf-perc bpf-env bpf-adsr bpf-perc res-env res-adsr
             res-perc phaser-env phaser-adsr phaser-perc crush-env
             crush-adsr crush-perc detune-env detune-adsr detune-perc
             pshift-env pshift-adsr pshift-perc fshift-env fshift-adsr
             fshift-perc pan-env pan-adsr pan-perc distort-env
             distort-adsr distort-perc fm echo step degrees
             alt slowcat stack fastcat euclid every-cycle)

;; --- Player Re-exports ---

(import-vars player
             metro cpm glide-cpm player-state at-metro gate-off
             update-mono-inst start-mono-inst at-metro-mono
             trigger-event trigger-single-event apply-swing cycle-n
             play-loop playing play! play-only! stop!)

;; --- Synths Re-exports ---
(defmacro def-additive!
  "Defines a new additive synth from a sequence of harmonic volume ratios."
  [name ratios & opts]
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

;; --- MIDI Re-exports ---

(import-vars midi
             midi-state reset-midi-state! scale-cc-val
             def-midi-cc! set-midi-cc! get-midi-cc-val midi-cc
             def-midi-pad! def-midi-pad-toggle!
             midi-debug! midi-debug?
             handle-midi-msg midi-in-devices midi-in-connect!
             midi-in-disconnect!
             midi-out-devices midi-out-connect!
             midi-out-disconnect!
             midi-send-cc! midi-set-pad-light!
             colors coord->note note->coord rgb
             light-on! light-grid! random-lights
             test-midi-out!)

;; --- Live Reloading ---

(defn reload!
  "Reloads all strudel-overtone namespaces in dependency order,
   re-compiles core re-exports, and refers them into target-ns (default *ns*)."
  ([]
   (reload! *ns*))
  ([target-ns]
   (let [namespaces '[strudel-overtone.util
                      strudel-overtone.pattern
                      strudel-overtone.synths
                      strudel-overtone.samples
                      strudel-overtone.player
                      strudel-overtone.midi
                      strudel-overtone.core]]
     (doseq [n namespaces]
       (require n :reload))
     (when target-ns
       (binding [*ns* (if (instance? clojure.lang.Namespace target-ns)
                        target-ns
                        (the-ns target-ns))]
         (refer 'strudel-overtone.core)))
     :reloaded)))

;; --- Main / Entry ---

(defn -main
  "Main entry point that connects to SuperCollider server and logs readiness."
  [& args]
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
             (lpf-env 5000)                  ; Envelope modulation depth = 5000 Hz
             (lpf-adsr 0.01 0.15 0.1 0.2)   ; Plucky filter sweep
             (adsr 0.01 0.2 0.8 0.1)        ; Volume envelope
             (resonance 0.1)))

  (stop!)
)
