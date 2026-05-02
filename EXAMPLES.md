# Strudel-Overtone Capabilities & Examples

This project brings Strudel-style live coding to Overtone, using Clojure data structures (vectors and keywords) for patterns with powerful SuperCollider synths.

## 1. Patterns & Structure

Patterns are created using `s` (sound) or `note`. They support nested vectors for subdivision and sets for simultaneous events (chords).

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
*   **`env`**: Choose between `:adsr` (default for melodic) and `:perc` (default for drums) envelopes.
*   **`def-additive!`**: Define custom additive synths using harmonic ratios.

### Envelope Examples

Every synth supports both ADSR and Percussive envelopes. You can shape your sound using parameters like `attack`, `decay`, `s-level`, `release`, `sustain`, and `legato`.

```clojure
;; 1. Slow ADSR Lead (Pads)
(play! :pad (-> (note [#{:c3 :e3 :g3}])
                (s :saw)
                (attack 1.0)   ; 1 second fade in
                (release 2.0)  ; 2 second fade out
                (legato 1.0))) ; Hold for the full rhythmic duration

;; 2. Percussive Pluck (forced percussive mode)
(play! :pluck (-> (note [:c4 :eb4 :g4])
                  (s :tb303)
                  (env :perc)     ; Force percussive envelope
                  (attack 0.001)  ; Sharp attack
                  (sustain 0.1)   ; Fast decay
                  (lpf 1000)))

;; 3. Snappy Drums
(play! :drums (-> (s [:kick :hh :sd :hh])
                  (sustain 0.05))) ; Make all drum hits very short and tight
```

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

## 6. Playback Control

*   `play! :name pattern`: Starts/updates a loop.
*   `play-only! :name pattern`: Stops everything else and plays this.
*   `stop! :name`: Stops a specific loop.
*   `stop!`: Stops all music.
*   `cpm 120`: Sets Cycles Per Minute (default is 4 beats per cycle).
*   `glide-cpm 80 4`: Smoothly transitions tempo over 4 cycles.
