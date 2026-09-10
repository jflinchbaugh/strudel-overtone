# Tutorial: Building Music with Strudel-Overtone

Welcome to **Strudel-Overtone**! This guide covers everything you need to know
to compose, design sounds, and perform live music with Clojure and SuperCollider.

Strudel-Overtone combines the musical pattern syntax and tidal-cycles philosophy
of [Strudel](https://strudel.cc) with the audio synthesis, MIDI connectivity,
and sample power of [Overtone](https://overtone.github.io).

---

## Table of Contents
1. [Quickstart & Environment Setup](#1-quickstart--environment-setup)
2. [Core Concepts: Cycles, Time, and Patterns](#2-core-concepts-cycles-time-and-patterns)
3. [Rhythm & Notation: Mini-Notation in Clojure Data](#3-rhythm--notation-mini-notation-in-clojure-data)
4. [Synthesizers & Instruments](#4-synthesizers--instruments)
5. [Additive Synthesis (`def-additive!`)](#5-additive-synthesis-def-additive)
6. [Envelopes & Filters (ADSR, Percussive, LPF Sweeps)](#6-envelopes--filters)
7. [Pitch, Chords, Scales, and Degrees](#7-pitch-chords-scales-and-degrees)
8. [Sample Management & Slicing](#8-sample-management--slicing)
9. [Pattern Transformations & Arrangement](#9-pattern-transformations--arrangement)
10. [LFOs, Signals, and Deterministic Randomness](#10-lfos-signals-and-deterministic-randomness)
11. [Expressive Inline Modifications](#11-expressive-inline-modifications)
12. [Monophonic Synths, Glides & Sidechain Ducking](#12-monophonic-synths-glides--sidechain-ducking)
13. [Hardware MIDI: Knobs, Pads, and Grid Lighting](#13-hardware-midi-knobs-pads-and-grid-lighting)
14. [Live Coding Workflow & Playback Control](#14-live-coding-workflow--playback-control)

---

## 1. Quickstart & Environment Setup

### Prerequisites
- Linux with PipeWire (or JACK) running.
- SuperCollider (`scsynth` / `supernova`).
- Clojure CLI tools installed.

### Starting the Audio Server & REPL
1. Start supernova and the nREPL server:
   ```bash
   ./supernova.sh
   ```
2. Connect your editor (Emacs CIDER, Calva, etc.) to the running nREPL port.
3. In your REPL or song buffer, require `strudel-overtone.core`:
   ```clojure
   (ns my-track
     (:require [strudel-overtone.core :refer :all]
               [overtone.core :as ov]))
   ```
4. Check or set the tempo in **Cycles Per Minute (CPM)**:
   ```clojure
   (cpm 30) ; 30 CPM = 120 BPM in standard 4-beat cycles
   ```

---

## 2. Core Concepts: Cycles, Time, and Patterns

Strudel-Overtone operates on the concept of **Cycles**:
- A **cycle** represents 1 measure of musical time (default 4 beats on the metronome).
- All patterns are relative to this cycle: an event from `time 0.0` to `duration 0.5`
  occupies the first half of the cycle.
- Time expands or contracts evenly within collections.

### Basic Patterns & Pattern Names
- `s`: creates a sound or percussion pattern.
- `note`: creates a pitched note pattern.
- `play!`: schedules or hot-swaps a named loop on the global metronome.
- `stop!`: halts a specific loop or all loops.

Each loop is identified by a unique keyword name (e.g., `:kick`, `:drums`,
`:bass`):
- Playing with a new name adds a parallel layer running in sync.
- Playing with an existing name hot-swaps the pattern seamlessly at the cycle
  boundary without restarting the metronome.
- The standard convention is to write the pattern name on a new line with
  the pattern expression:

```clojure
;; Play a simple 4-on-the-floor kick
(play!
 :kick (s [:kick :kick :kick :kick]))

;; Add hats in parallel under a distinct pattern name
(play!
 :hats (s [:- :hat :- :hat]))

;; Stop only the kick
(stop! :kick)

;; Stop all music
(stop!)
```

---

## 3. Rhythm & Notation: Mini-Notation in Clojure Data

Instead of parsing complex string DSLs, Strudel-Overtone uses idiomatic Clojure vectors,
sets, and keywords.

### Subdivisions (Vectors)
Vectors divide time equally among their elements:
```clojure
;; 4 equal steps: [step1 step2 step3 step4]
(s [:kick :snare :kick :snare])

;; Nested vectors subdivide their step:
;; Step 2 is split into two fast snares
(s [:kick [:snare :snare] :kick :snare])
```

### Rests (`:-`)
Use keyword `:-` to indicate silence:
```clojure
;; Offbeat hats: rest, hat, rest, hat
(s [:- :hat :- :hat])
```

### Simultaneous Events / Chords (Sets or `simul`)
Sets `#{...}` play all elements at the same instant:
```clojure
;; Simultaneous kick and hat on the 1
(s [#{:kick :hat} :hat :snare :hat])

;; Triad chord on beat 1, pause, dyad on beat 3
(note [#{:c3 :e3 :g3} :- #{:f3 :a3} :-])
```

### Euclidean Rhythms (`euclid`)
Distribute `k` hits across `n` steps using the Bjorklund algorithm:
```clojure
;; Tresillo rhythm: 3 hits across 8 steps
(s (euclid 3 8 :dub-kick))

;; 5 hits across 8 steps, rotated by 1 step
(s (euclid 5 8 :hat :- 1))
```

---

## 4. Synthesizers & Instruments

Strudel-Overtone includes built-in instruments for drums, bass, leads, and pads:

| Category | Synths / Sounds |
| :--- | :--- |
| **Drums** | `:kick`, `:snare`, `:hat`, `:clap`, `:dub-kick`, `:dance-kick` |
| **Melodic** | `:saw`, `:sine`, `:square`, `:tri`, `:supersaw`, `:tb303`, `:mooger`, `:fm` |
| **Acoustic/String**| `:ks-stringer` (Karplus-Strong plucked string) |
| **Noise/Texture** | `:white`, `:pink`, `:brown`, `:gray`, `:crackle`, `:dust`, `:dust2` |

```clojure
(play! :melody
       (-> (note [:c4 :eb4 :g4 :bb4])
           (s :supersaw)
           (gain 0.7)
           (lpf 2500)))
```

---

## 5. Additive Synthesis (`def-additive!`)

Build rich, custom harmonic instruments by specifying amplitude ratios for harmonics:

```clojure
;; 1. Classic Drawbar Organ (all integer harmonics 1, 2, 3, 4, 5, 6)
(def-additive! :church-organ [1.0 0.8 0.6 0.4 0.3 0.1])

;; 2. Hollow Woodwind (odd harmonics only: 1, 3, 5, 7)
(def-additive! :pan-flute [1.0 0.0 0.6 0.0 0.3] :step 2)

;; 3. Metallic Chime / Bell (Inharmonic golden ratio partials)
(def-additive! :golden-bell [1.0 0.7 0.5 0.3] :step 1.618)
```

Play your additive synth with standard envelopes and effects:
```clojure
(play! :chimes
       (-> (note [:c5 :eb5 :g5 :c6])
           (s :golden-bell)
           (perc 0.001 0.6)
           (room 0.5)
           (echo 0.25 4)))
```

---

## 6. Envelopes & Filters

### Volume Envelopes
Every synth supports both **ADSR** and **Percussive** modes:
- `(env :adsr)` (default for melodic synths): uses `(adsr attack decay sustain release)`
- `(env :perc)` (default for drums): uses `(perc attack release)`

```clojure
;; Plucky percussive saw
(play! :plucks
       (-> (note [:c4 :d4 :eb4 :g4])
           (s :saw)
           (env :perc)
           (perc 0.005 0.15)))

;; Smooth ambient pad
(play! :swell
       (-> (note [#{:c3 :g3 :c4 :eb4}])
           (s :mooger)
           (adsr 1.0 0.5 0.8 1.5)))
```

### Filter Envelopes (Cutoff Sweeps)
Shape dynamic timbre using filter envelope parameters:
- `(lpf cutoff-hz)`: Base low-pass filter frequency.
- `(lpf-env depth-hz)`: Envelope sweep depth added to base cutoff.
- `(lpf-adsr att dec sus rel)`: Dedicated ADSR for filter modulation.
- `(resonance amount)`: Filter resonance (0.0 to 1.0).

```clojure
;; Classic 303 acid filter sweep
(play! :acid
       (-> (note [:c2 :c3 :eb2 :g2])
           (s :tb303)
           (lpf 400)
           (lpf-env 3500)
           (lpf-adsr 0.01 0.15 0.1 0.1)
           (resonance 0.85)))
```

### Sound Design Shortcuts
- `(acid cutoff res depth)`: Sets up an acid filter in one call.
- `(drive distort crush)`: Distortion and bitcrushing combined.
- `(space room delay repeats)`: Reverb and echo combined.

```clojure
(play! :gritty
       (-> (note [:c3 :eb3 :f3 :g3])
           (s :saw)
           (drive 0.5 0.2)
           (space 0.4 0.25 4)))
```

---

## 7. Pitch, Chords, Scales, and Degrees

### Note Naming
Always use keyword notes: `:c4`, `:eb3`, `:f#2`, `:bb4`.

### Degrees in a Scale
Sequence melodies by modal scale degrees:
```clojure
(play! :scale-walk
       (-> (note :c3)
           (degrees :dorian [1 2 3 4 5 7 8])
           (s :saw)
           (lpf 1800)))
```

### Chords with Overtone Helpers
Leverage `overtone.core` chord functions directly:
```clojure
;; Generate chord tones as simultaneous sets
(play! :chords
       (-> (note [(ov/chord :c3 :minor7)
                  (ov/chord :f3 :minor7)
                  (ov/chord :g3 :7)])
           (s :saw)
           (slow 2)
           (room 0.4)))
```

---

## 8. Sample Management & Slicing

### Loading Samples
```clojure
;; Load local wav files
(load-sample! :kick "samples/99sounds/kick.wav")

;; Load from Freesound.org by ID
(load-freesound! :amen 20933)
```

### Slicing Loops
Break loop samples into playable slices:
```clojure
;; Slice by normalized position (0.0 to 1.0)
(slice-sample! :break-hit :amen 0.0 0.125)

;; Slice by milliseconds
(slice-sample-ms! :snare-slice :amen 250 500)

;; Play sliced samples rhythmically
(play! :breakbeat (s [:break-hit :snare-slice :break-hit [:snare-slice :snare-slice]]))
```

---

## 9. Pattern Transformations & Arrangement

### Time & Sequence Modifiers
- `(fast n)` / `(slow n)`: Speed up or slow down patterns.
- `(rev)`: Reverse events in each cycle.
- `(alt v1 v2 ...)`: Alternate values across successive cycles.
- `(slowcat p1 p2 ...)`: Concatenate multiple cycles sequentially.
- `(stack p1 p2 ...)`: Layer multiple patterns concurrently.
- `(fastcat p1 p2 ...)`: Compress multiple patterns into a single cycle.
- `(every-cycle n f)`: Apply transformation `f` every `n` cycles.
- `(sometimes f)`: Probabilistically apply `f` (50% chance).
- `(degrade amount)`: Randomly drop events.

```clojure
;; Evolving drum groove
(play! :groove
       (-> (s [:kick (alt :snare :clap) :kick (alt :snare [:snare :snare])])
           (every-cycle 4 rev)
           (gain 0.85)))
```

---

## 10. LFOs, Signals, and Deterministic Randomness

Parameters accept functions `(fn [cycle-t param-key] ...)` that evaluate dynamically:

### Continuous LFO Signals
- `sine-sig`, `saw-sig`, `tri-sig`, `square-sig`, `cosine-sig`
- Signature: `(sine-sig cycles-per-pattern min-val max-val)`

```clojure
;; Pan sweeping back and forth every 2 cycles
(play! :sweeping-lead
       (-> (note [:c4 :e4 :g4 :b4])
           (s :saw)
           (pan (sine-sig 0.5 -0.8 0.8))
           (lpf (sine-sig 0.25 400 4000))))
```

### Deterministic Randomness
- `(seed! n)`: Set seed for reproducible performances.
- `(irand low high)`: Random integer stream.
- `(srand low high)`: Random float stream.
- `(choose coll)`: Pick elements randomly per step.

```clojure
(play! :generative
       (-> (note (choose [:c3 :eb3 :f3 :g3 :bb3 :c4]))
           (fast 2)
           (s :sine)
           (perc 0.01 0.2)
           (room 0.5)))
```

---

## 11. Expressive Inline Modifications

Decorate individual notes, sub-sequence vectors, or chords directly inline:

```clojure
;; 1. Individual Note Nuance:
;; e.g. An accented note with a pitch slide
(play! :lead
       (-> (note [:c4
                  (-> :eb4 (gain 0.4) (glide 0.1))
                  (-> :g4 (gain 1.0) (lpf 3500))
                  :c5])
           (s :tb303)
           (mono)))

;; 2. Vector Sub-sequence Dynamics:
(play! :grouped
       (-> (note [:c3
                  (-> [:eb3 :g3 :bb3] (gain 0.5) (lpf 800))
                  :c4])
           (s :saw)))

;; 3. Dynamic Chord Voicings:
(play! :pads
       (-> (note [#{:c3 :g3 :c4}
                  (-> #{:eb3 :bb3 :eb4} (gain 0.6) (lpf 1200))
                  #{:f3 :c4 :f4}])
           (s :mooger)
           (slow 2)))
```

---

## 12. Monophonic Synths, Glides & Sidechain Ducking

### Monophonic Mode & Portamento Glide
Use `(mono)` and `(glide time-seconds)` for legato basslines and synth leads:
```clojure
(play! :mono-bass
       (-> (note [:c2 :c2 :eb2 :g2 :f2 :_])
           (s :tb303)
           (mono)
           (glide 0.06)
           (lpf 800)))
```

### Sidechain Ducking
Trigger compression on bass and leads whenever the kick hits:
```clojure
;; Kick sends duck triggers on bus
(play! :kick
       (-> (s [:kick :_ :kick :_])
           (duck-trigger 1)))

;; Bass reacts to duck trigger
(play! :ducking-bass
       (-> (note [:c2 :eb2 :f2 :g2])
           (s :saw)
           (lpf 400)
           (duck 0.85))) ; Dips volume by 85% when kick triggers
```

---

## 13. Hardware MIDI: Knobs, Pads, and Grid Lighting

Integrate your external hardware MIDI controllers for hands-on control:

### Connecting & Discovery
```clojure
;; Discover inputs
(midi-in-devices)
(midi-in-connect!)

;; Enable debug mode to see CC and pad note numbers in the console
(midi-debug! true)
```

### Mapping Knobs (CC)
```clojure
;; Map CC 74 to a low-pass filter cutoff with exponential curve
(def-midi-cc! :cutoff 74 :min 200 :max 10000 :curve :exp :default 800)

(play! :lead
       (-> (note [:c4 :g4 :bb4 :c5])
           (s :tb303)
           (lpf (midi-cc :cutoff))))
```

### Performance Pads
```clojure
;; Pad 36 toggles kick loop
(def-midi-pad-toggle! 36
  (fn [_] (play! :kick (s (euclid 4 8 :kick))))
  (fn [_] (stop! :kick)))

;; Pad 39 panic stop
(def-midi-pad! 39 (fn [_] (stop!)))
```

### Grid & Pad Lights (`light-grid`, `pad-light`)
Sequence Launchpad or SmartPAD colors on the metronome:
```clojure
(midi-out-connect!)

;; Cycle grid colors sequentially across the pattern
(play! :light-show
       (-> (light-grid [:red :blue :green :yellow])
           (fast 2)))

;; Sync lights to note rests or beats
(play! :pads
       (-> (note [:c3 :e3 :g3 :b3])
           (pad-light :cyan)))
```

---

## 14. Live Coding Workflow & Playback Control

| Function | Description |
| :--- | :--- |
| `(play! :id pattern)` | Start or hot-swap a running pattern loop |
| `(play-only! :id pat ...)` | Stop all other patterns and isolate these |
| `(stop! :id)` | Stop a specific pattern loop |
| `(stop!)` | Stop all playback immediately |
| `(playing)` | List all currently active loops |
| `(cpm n)` | Set tempo in Cycles Per Minute |
| `(glide-cpm target cycles)` | Smoothly transition tempo over N cycles |
| `(reload!)` | Hot-reload all strudel-overtone source code |

Enjoy building music with Strudel-Overtone! For practical, runnable examples,
explore the accompanying [`tutorial_walkthrough.clj`](file:///home/john/workspace/strudel-overtone/src/strudel_overtone/tutorial_walkthrough.clj).
