(ns strudel-overtone.midi-showcase
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

;; Showcase: MIDI Controller Integration (Knobs, Faders & Pads)
;; Demonstrates real-time parameter tweaking with CC knobs and
;; performance triggering with hardware pads.

(comment
  (stop!)

  ;; --- 1. Connect Your Controller ---
  ;; List detected MIDI input sources
  (midi-in-devices)

  ;; Connect to first available device, or specify name: (midi-in-connect! "SmartPAD [hw:3,0,0]")
  (midi-in-connect!)

  ;; --- 2. Identify Knobs & Pads (Debug Logger) ---
  ;; Turn on debug logging to see incoming CC and Note events in the REPL console:
  (midi-debug! true)
  ;; (Twist a knob to see its CC number, or hit a pad to see its Note number)
  ;; Turn off debug logging when you're done discovering controls:
  (midi-debug! false)

  ;; --- 3. Configure Knobs & Faders (MIDI CC) ---
  ;; CC 74: Low-pass filter cutoff with exponential curve (200 Hz -> 10 kHz)
  (def-midi-cc! :cutoff 3 :min 200 :max 10000 :curve :exp :default 800)

  ;; CC 71: Filter resonance (0.05 -> 1.0)
  (def-midi-cc! :res 71 :min 0.05 :max 1.0 :default 0.2)

  ;; CC 91: Reverb room depth (0.0 -> 0.8)
  (def-midi-cc! :verb 2 :min 0.0 :max 0.8 :default 0.1)

  ;; CC 1: Mod Wheel controlling distortion or LFO depth
  (def-midi-cc! :mod 1 :min 0.0 :max 0.8 :default 0.0)

  ;; --- 4. Bind Hardware Drum Pads ---
  ;; Pad 36 (C1): Toggle Kick loop on/off
  (def-midi-pad-toggle! 36
    (fn [_] (play! :kick (-> (s (euclid 3 8 :dub-kick)) (gain 1.0) (duck-trigger 1))))
    (fn [_] (stop! :kick)))

  ;; Pad 37 (C#1): Toggle Snare loop on/off
  (def-midi-pad-toggle! 37
    (fn [_] (play! :snare (-> (s [:- :snare :- (alt :snare :clap)]) (gain 0.8))))
    (fn [_] (stop! :snare)))

  ;; Pad 38 (D1): Toggle 7/16 Euclidean Hats on/off
  (def-midi-pad-toggle! 38
    (fn [_] (play! :hat (-> (s (euclid 7 16 :hat)) (gain 0.6) (room (midi-cc :verb)))))
    (fn [_] (stop! :hat)))

  ;; Pad 39 (D#1): Panic / Stop all music
  (def-midi-pad! 39
    (fn [_] (stop!)))

  ;; --- 5. Live Acid Jam Controlled by Knobs ---
  ;; Twist knobs mapped to CC 74 (cutoff) and CC 71 (res) while playing!
  (play! :acid
         (-> (note (euclid 5 8 :c2 :- 1))
             (s :tb303)
             (lpf (midi-cc :cutoff))       ; Real-time knob cutoff sweep
             (resonance (midi-cc :res))    ; Real-time resonance knob
             (distort (midi-cc :mod))      ; Mod wheel distortion
             (lpf-env 3000)
             (lpf-adsr 0.01 0.15 0.1 0.1)
             (adsr 0.01 0.2 0.7 0.1)
             (duck 0.7)))

  ;; --- 5. Atmospheric Synth Layer with Reverb Knob ---
  (play! :pad
         (-> (note [[:c3 :eb3 :g3] [:f3 :ab3 :c4]])
             (s :saw)
             (lpf (midi-cc :cutoff))
             (room (midi-cc :verb))        ; Twist CC 91 for reverb swells
             (adsr 0.5 0.5 0.7 1.0)
             (gain 0.35)))

  ;; --- 6. Manual Testing (Virtual Controller) ---
  ;; If you do not have hardware plugged in right now, simulate CC messages:
  (set-midi-cc! 74 100) ; Opens filter cutoff
  (set-midi-cc! 74 20)  ; Closes filter cutoff
  (set-midi-cc! 91 110) ; Turns up reverb

  ;; Inspect current value of a knob control at the REPL:
  @(midi-cc :cutoff)
  @(midi-cc :verb)

  (stop!)

  ;; --- 7. MIDI Out Feedback (Knobs, Motorized Faders, & Pad Lights) ---
  ;; List available MIDI output sinks
  (midi-out-devices)

  ;; Connect to first available output or specify device name
  (midi-out-connect!)

  ;; Send feedback value 100 to CC 74 (e.g. knob LED ring or motorized fader)
  (midi-send-cc! :cutoff 100)
  (midi-send-cc! 74 64)

  (midi-send-cc! 1 127)

  ;; Set pad 36 light (velocity 127 = full brightness / color index)
  (midi-set-pad-light! 54 127)

  ;; Turn off pad 36 light
  (midi-set-pad-light! 36 0)

  ;; --- 8. Grid Pad Lighting & Custom RGB Colors ---
  ;; Light up a single pad with a named color or custom RGB
  (light-on! [0 0] :red)
  (light-on! [0 1] :green)
  (light-on! [0 2] (rgb 0.0 0.0 1.0))

  (light-on! [0 3] (rgb 255 128 0))

  ;; Fill the entire 8x8 grid with random colors
  (light-grid! random-lights)

  (light-grid! coord->note)

  (light-grid! (constantly 0))

  ;; Light up an alternating checkerboard pattern
  (light-grid! (fn [r c] (if (even? (+ r c)) :blue :yellow)))

  ;; Clear the entire grid (black)
  (light-grid! (fn [_ _] :black))

  ;; Disconnect when done
  (midi-in-disconnect!)
  (midi-out-disconnect!)

  (stop!)


  (reload!)

  ;; --- 9. Pattern-Driven Grid & Pad Lights ---
  ;; Playhead moving across columns
  (play! :playhead
         (-> (s [:dub-kick :- :snare :-])
             (light-grid
              (fn [time]
                (fn [row col]
                  (if (= (int (mod (* time 4) 8)) col)
                    :green
                    :black))))))

  ;; Euclidean rhythm triggering random grid light pulses
  (play! :random-pulse
         (-> (euclid 5 8 :hat)
             (light-grid  [_t] random-lights)))

  ;; --- 10. pad-light Instrument & Modifiers ---
  ;; Sequence individual pad hits directly:
  (play! :pad-seq
         (pad-light [[0 0] [0 1] [0 2] [0 3]] :cyan))

  ;; Alternate colors across pads:
  (play! :pad-rainbow
         (pad-light [[0 0] [1 1] [2 2] [3 3]] [:red :yellow :green :cyan]))

  ;; Highlight notes of an existing melody on pads:
  (play! :melody-lights
         (-> (note [:c3 :e3 :g3 :b3])
             (s :saw)
             (pad-light :yellow)))

  ;; Pair audio drum pattern with specific pad lights:
  (play! :drum-lights
         (-> (s [:dub-kick :- :snare :-])
             (pad-light [[0 0] :- [0 1] :-] :red)))

  (play! :melody-lights
         (-> (note [:c3 :e3 :g3 :b3])
             (s :saw)
             (pad-light :yellow)))

  (play! :pad-rainbow
              (pad-light [[0 0] [1 1] [2 2] [3 3]] [:red :yellow :green :cyan]))

  (play! :pad-seq
         (pad-light [[0 0] [0 1] [0 2] [0 3]] :cyan))


  (light-grid! (constantly 0))

  (stop!)

  (reload!)

  ;; Turn off all lights and stop
  (stop!)
  (light-grid! (fn [_ _] :black))
  (midi-in-disconnect!)
  (midi-out-disconnect!)

  (stop!)

)
