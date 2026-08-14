# Strudel-Overtone Capabilities & Examples

This project brings Strudel-style live coding to Overtone, using Clojure
data structures (vectors and keywords) for patterns with powerful
SuperCollider synths.

## 1. Patterns & Structure

Patterns are created using `s` (sound) or `note`. They support nested vectors
for subdivision and sets for simultaneous events (chords).

```clojure
;; Simple beat (vectors for timing)
(s [:bd :sd :bd [:sd :sd]])

;; Simultaneous notes (sets or simul)
(note [#{:c3 :e3 :g3}])
(note [(simul [:c3 :e3 :g3])])

;; Functional patterns (LFOs)
(gain (sine 1 0.2 0.8)) ; Sine LFO between 0.2 and 0.8 gain
```

## 2. Structural Modifiers

Modify the timing and structure of your loops.

*   `fast n`: Speeds up the pattern by factor n.
*   `slow n`: Slows down the pattern by factor n.
*   `rev`: Reverses the events within a cycle.
*   `alt v1 v2 ...`: Alternates between values cycle-by-cycle.
*   `every-cycle n f`: Applies function f to pattern every n cycles.
*   `euclid k n [hit] [rest] [rot]`: Generates a Euclidean rhythm vector.
*   `slowcat p1 p2 ...`: Concatenates patterns in time (sequential).
*   `stack p1 p2 ...`: Layers patterns (simultaneous).
*   `fastcat p1 p2 ...`: Squeezes patterns into a single cycle.
*   `ribbon offset len`: Loops a specific segment of a pattern.
*   `sometimes func`: Randomly applies a function to events.
*   `delay-cycles n`: Delays the start of a pattern by n cycles.
*   `stop-after n`: Automatically stops the pattern after n cycles.

## 3. Modular Synths

A variety of built-in synths are available:

*   **Drums**: `:kick`, `:snare`, `:hat`, `:clap`, `:dub-kick`, `:dance-kick`.
*   **Melodic**: `:sine`, `:saw`, `:tri`, `:square`, `:fm`, `:supersaw`, `:tb303`.
*   **Special**: `:mooger` (Moog filter), `:ks-stringer` (Karplus-Strong), `:crackle`, `:white` noise.

```clojure
(play! :bass (-> (note [:c2 :g2]) (s :saw) (lpf 500)))
```

## 4. Sample Management

Load and slice your own samples.

```clojure
;; Load a local file
(load-sample! :my-kick "path/to/kick.wav")

;; Load from Freesound
(load-freesound! :amen 20933)

;; Create slices
(slice-sample! :kick :amen 0 0.1)
```

## 5. Advanced Parameter Handling

*   **`overlay`**: Apply a parameter pattern without splitting the base rhythm.
*   **`mono` & `glide`**: Enable monophonic mode with legato portamento.
*   **`duck` & `duck-trigger`**: Easy sidechain compression.
*   **`env`**: Choose between `:adsr` (default for melodic)
               and `:perc` (default for drums) envelopes.
*   **`def-additive!`**: Define custom additive synths using harmonic ratios.


```clojure
;; 1. Standard Organ (integer harmonics: 1, 2, 3, 4)
(def-additive! :organ [1.0 0.5 0.33 0.25])

;; 2. Hollow Sound (odd harmonics: 1, 3, 5, 7)
(def-additive! :hollow [1.0 0.6 0.4 0.2] :step 2)

;; 3. Metallic/Bell (inharmonic: 1, 2.5, 4, 5.5)
(def-additive! :bell [1.0 0.8 0.6 0.4] :step 1.5)

;; Play it
(play! :melody (-> (note [:c3 :e3 :g3]) (s :organ) (attack 0.5)))
```

### Envelope Examples

Every synth supports both volume and filter envelopes (`:adsr` or `:perc`).
You can shape volume with `(adsr attack decay sustain-level release)` or `(perc attack sustain)`,
and modulate filter cutoff with `(lpf-env depth-hz)`, `(lpf-adsr ...)` or `(lpf-perc ...)`.

```clojure
;; 1. Classic Acid 303 Bass (Filter ADSR Sweep)
(play! :acid (-> (note [:c2 :c3 :c2 :eb2 :g2])
                 (s :tb303)
                 (lpf 300)                      ; Resting base cutoff = 300 Hz
                 (lpf-env 5000)                 ; Peak envelope sweep depth = 5000 Hz
                 (lpf-adsr 0.01 0.15 0.1 0.2)   ; Plucky filter sweep (att dec sus rel)
                 (adsr 0.01 0.2 0.8 0.1)        ; Volume envelope
                 (resonance 0.1)))

;; 2. Downward Filter Sweep (Bright Pluck decaying to warm tone)
(play! :pluck (-> (note [:c3 :g3 :c4 :eb4])
                  (s :saw)
                  (lpf 6000)                    ; Start bright at 6000 Hz cutoff
                  (lpf-env -5000)               ; Sweep DOWN by 5000 Hz as envelope decays
                  (lpf-adsr 0.005 0.25 0.0 0.1) ; Fast decay to 0 sustain
                  (adsr 0.005 0.3 0.6 0.1)))

;; 3. Slow Ambient Pad with Soft Filter Envelope
(play! :pad (-> (note [#{:c3 :eb3 :g3 :bb3}])
                (s :saw)
                (lpf 500)                       ; Dark baseline cutoff
                (lpf-env 3000)                  ; Gentle 3kHz filter swell
                (lpf-adsr 1.5 2.0 0.7 2.0)      ; Slow filter swell
                (adsr 1.0 1.0 0.8 2.0)          ; Slow volume swell
                (legato 1.0)))
```
### Alternating & Combining Patterns

Use `alt` to swap values every cycle, or `slowcat` to chain entire patterns.

```clojure
;; 1. Alternating sound every cycle
(play! :beat (s [:bd (alt :sd :cp) :bd :hh]))

;; 2. Concatenating different patterns
(play! :song (slowcat (s [:bd :sd])
                      (s [:hh :hh :hh :hh])
                      (-> (note [:c3 :e3 :g3 :b3]) (s :saw))))
```

## 6. Playback Control

*   `(play! :name pattern)`: Starts/updates a loop.
*   `(play-only! :name pattern)`: Stops everything else and plays this.
*   `(stop! :name)`: Stops a specific loop.
*   `(stop!)`: Stops all music.
*   `(cpm 120)`: Sets Cycles Per Minute (default is 4 beats per cycle).
*   `(glide-cpm 80 4)`: Smoothly transitions tempo over 4 cycles.
