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

```clojure
;; Overlay gain without breaking long pads
(-> (note (repeat 4 :c3)) (s :saw) (gain (overlay [1 0.5 0.8])))

;; Mono glide
(-> (note [:c3 :e3]) (mono) (glide 0.1) (s :saw))
```

## 6. Playback Control

*   `play! :name pattern`: Starts/updates a loop.
*   `play-only! :name pattern`: Stops everything else and plays this.
*   `stop! :name`: Stops a specific loop.
*   `stop!`: Stops all music.
*   `cpm 120`: Sets Cycles Per Minute (default is 4 beats per cycle).
*   `glide-cpm 80 4`: Smoothly transitions tempo over 4 cycles.
