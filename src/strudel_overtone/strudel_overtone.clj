(ns strudel-overtone.strudel-overtone
  (:require [overtone.core :as ov :refer :all
             :exclude [note lpf decay distort hpf bpf vibrato defsynth]]
            [taoensso.telemere :as tel]
            [clojure.string :as str])
  (:import [java.time ZoneId]
           [java.time.format DateTimeFormatter]))

(defmacro s-max [min-val val]
  `(ov/clip ~val ~min-val 20000))

(defmacro defsynth [name args & body]
  `(ov/defsynth ~name ~args
     (line:kr 0 0 60 FREE)
     ~@body))

;; --- Synths ---

(defonce duck-bus (control-bus))

(defmacro def-strudel-synth [name extra-args & body]
  (let [common-args '[amp 1 sustain 0.2 lpf 2000 resonance 0.1 pan 0
                      crush 0 distort 0
                      hpf 0 bpf -1 room 0 delay 0 repeats 4
                      duck 0 duck-trigger 0 duck-attack 0.001 duck-release 0.2
                      room-size 0.5 damp 0.5]
        adsr-args   '[attack 0.01 decay 0.1 s-level 0.5 release 0.3]
        perc-args   '[attack 0.01]
        ;; Helper to build the synth definition
        make-synth (fn [suffix env-gen-form args]
                     `(ov/defsynth ~(symbol (str name suffix))
                        ~(into (vec (concat extra-args common-args)) args)
                        (let [~'env ~env-gen-form
                              ;; Trigger Sidechain
                              _# (let [trig-env# (env-gen (perc ~'duck-attack ~'duck-release) :level-scale ~'duck-trigger)]
                                   (out:kr duck-bus trig-env#))
                              ;; Read Sidechain
                              ~'duck-env (in:kr duck-bus)
                              ~'amp-duck (ov/clip (~'- 1 (~'* ~'duck ~'duck-env)) 0 1)
                              ~'snd (do ~@body)
                              ;; Effect Chain
                              ~'filt (ov/hpf ~'snd (~'s-max 20 ~'hpf))
                              ~'filt (select (~' > ~'bpf 0)
                                       [~'filt
                                        (ov/bpf ~'filt (~'s-max 20 ~'bpf) 1)])
                              ~'dst (ov/distort
                                      (~' * ~'filt
                                       (ov/dbamp (~' * ~'distort 24))))
                              ~'crs (decimator ~'dst
                                      (ov/lin-lin ~'crush 0 1 44100 2000)
                                      (ov/lin-lin ~'crush 0 1 24 4))
                              ~'gated (~' * ~'crs ~'env)
                              ~'dly (select (~' > ~'delay 0)
                                      [~'gated
                                       (~' + ~'gated
                                        (comb-n ~'gated 0.5
                                          (~'s-max 0.0001 ~'delay)
                                          (~' * ~'delay ~'repeats)))])
                              ~'reverbed (free-verb ~'dly ~'room ~'room-size ~'damp)
                              _# (detect-silence
                                   ~'reverbed
                                   :amp 0.0001
                                   :time 0.2
                                   :action FREE)]
                          (out 0 (pan2 (~' * ~'reverbed ~'amp ~'amp-duck) ~'pan)))))]
    `(do
       ~(make-synth "-adsr"
                    `(env-gen (adsr ~'attack ~'decay ~'s-level ~'release)
                              :gate (line:kr 1 0 ~'sustain)
                              :action NO-ACTION)
                    adsr-args)
       ~(make-synth "-perc"
                    `(env-gen (perc ~'attack ~'sustain) :action NO-ACTION)
                    perc-args))))

;; --- Logging ---

(def ^:private time-formatter
  (-> (DateTimeFormatter/ofPattern "HH:mm:ss.SSS")
      (.withZone (ZoneId/systemDefault))))

(tel/stop-handlers!)
(tel/add-handler! :console (tel/handler:console
                             {:output-fn
                              (fn [signal]
                                (let [{:keys [inst msg_ data]} signal]
                                  (str (.format time-formatter inst) " "
                                       (or (force msg_) data)
                                       "\n")))}))

(defsynth kick [amp 1 sustain 0.3 freq 60 lpf 3000 pan 0
                crush 0 distort 0
                hpf 0 bpf -1 room 0 delay 0 repeats 4
                duck 0 duck-trigger 0 duck-attack 0.001 duck-release 0.2
                room-size 0.5 damp 0.5]
  (let [env (env-gen (perc 0.01 sustain) :action NO-ACTION)
        ;; Trigger Sidechain
        _ (let [trig-env (env-gen (perc duck-attack duck-release) :level-scale duck-trigger)]
            (out:kr duck-bus trig-env))
        ;; Read Sidechain
        duck-env (in:kr duck-bus)
        amp-duck (ov/clip (- 1 (* duck duck-env)) 0 1)

        snd (ov/lpf (sin-osc (line:kr (* 2 freq) freq 0.1)) lpf)
        filt (ov/hpf snd (s-max 20 hpf))
        filt (select (> bpf 0) [filt (ov/bpf filt (s-max 20 bpf) 1)])
        dst (ov/distort (* filt (ov/dbamp (* distort 24))))
        crs (decimator
              dst
              (ov/lin-lin crush 0 1 44100 2000)
              (ov/lin-lin crush 0 1 24 4))
        gated (* crs env)
        dly (select (> delay 0)
              [gated
               (+ gated
                 (comb-n
                   gated
                   0.5
                   (s-max 0.0001 delay)
                   (* delay repeats)))])
        reverbed (free-verb dly room room-size damp)
        _ (detect-silence reverbed :amp 0.0001 :time 0.2 :action FREE)]
    (out 0 (pan2 (* reverbed amp amp-duck) pan))))

(defsynth snare [amp 1 sustain 0.2 freq 200 lpf 3000 pan 0
                 crush 0 distort 0
                 hpf 0 bpf -1 room 0 delay 0 repeats 4
                 duck 0 duck-trigger 0 duck-attack 0.001 duck-release 0.2
                 room-size 0.5 damp 0.5]
  (let [env (env-gen (perc 0.01 sustain) :action NO-ACTION)
        ;; Trigger Sidechain
        _ (let [trig-env (env-gen (perc duck-attack duck-release) :level-scale duck-trigger)]
            (out:kr duck-bus trig-env))
        ;; Read Sidechain
        duck-env (in:kr duck-bus)
        amp-duck (ov/clip (- 1 (* duck duck-env)) 0 1)

        noise (ov/lpf (white-noise) lpf)
        snd (+ (* 0.5 (sin-osc freq)) (* 0.8 noise))
        filt (ov/hpf snd (s-max 20 hpf))
        filt (select (> bpf 0) [filt (ov/bpf filt (s-max 20 bpf) 1)])
        dst (ov/distort (* filt (ov/dbamp (* distort 24))))
        crs (decimator
              dst
              (ov/lin-lin crush 0 1 44100 2000)
              (ov/lin-lin crush 0 1 24 4))
        gated (* crs env)
        dly (select (> delay 0)
              [gated
               (+ gated
                 (comb-n
                   gated
                   0.5
                   (s-max 0.0001 delay)
                   (* delay repeats)))])
        reverbed (free-verb dly room room-size damp)
        _ (detect-silence reverbed :amp 0.0001 :time 0.2 :action FREE)]
    (out 0 (pan2 (* reverbed amp amp-duck) pan))))

(defsynth hat [amp 1 sustain 0.1 freq 8000 lpf 6000 pan 0
               crush 0 distort 0
               hpf 0 bpf -1 room 0 delay 0 repeats 4
               duck 0 duck-trigger 0 duck-attack 0.001 duck-release 0.2
               room-size 0.5 damp 0.5]
  (let [env (env-gen (perc 0.001 sustain) :action NO-ACTION)
        ;; Trigger Sidechain
        _ (let [trig-env (env-gen (perc duck-attack duck-release) :level-scale duck-trigger)]
            (out:kr duck-bus trig-env))
        ;; Read Sidechain
        duck-env (in:kr duck-bus)
        amp-duck (ov/clip (- 1 (* duck duck-env)) 0 1)

        snd (ov/hpf (white-noise) lpf)
        filt (ov/hpf snd (s-max 20 hpf))
        filt (select (> bpf 0) [filt (ov/bpf filt (s-max 20 bpf) 1)])
        dst (ov/distort (* filt (ov/dbamp (* distort 24))))
        crs (decimator
              dst
              (ov/lin-lin crush 0 1 44100 2000)
              (ov/lin-lin crush 0 1 24 4))
        gated (* crs env)
        dly (select
              (> delay 0)
              [gated
               (+ gated
                 (comb-n
                   gated
                   0.5
                   (s-max 0.0001 delay)
                   (* delay repeats)))])
        reverbed (free-verb dly room room-size damp)
        _ (detect-silence reverbed :amp 0.0001 :time 0.2 :action FREE)]
    (out 0 (pan2 (* reverbed amp amp-duck) pan))))

(defsynth clap [amp 1 sustain 0.1 freq 1200 lpf 1500 resonance 0.2 pan 0
                crush 0 distort 0
                hpf 0 bpf -1 room 0 delay 0 repeats 4
                duck 0 duck-trigger 0 duck-attack 0.001 duck-release 0.2
                room-size 0.5 damp 0.5]
  (let [env (env-gen (perc 0.005 sustain) :action NO-ACTION)
        ;; Trigger Sidechain
        _ (let [trig-env (env-gen (perc duck-attack duck-release) :level-scale duck-trigger)]
            (out:kr duck-bus trig-env))
        ;; Read Sidechain
        duck-env (in:kr duck-bus)
        amp-duck (ov/clip (- 1 (* duck duck-env)) 0 1)

        snd (ov/bpf (white-noise) freq resonance)
        filt (ov/hpf snd (s-max 20 hpf))
        filt (select (> bpf 0) [filt (ov/bpf filt (s-max 20 bpf) 1)])
        dst (ov/distort (* filt (ov/dbamp (* distort 24))))
        crs (decimator
              dst
              (ov/lin-lin crush 0 1 44100 2000)
              (ov/lin-lin crush 0 1 24 4))
        gated (* crs env)
        dly (select (> delay 0)
              [gated
               (+ gated
                 (comb-n
                   gated
                   0.5
                   (s-max 0.0001 delay)
                   (* delay repeats)))])
        reverbed (free-verb dly room room-size damp)
        _ (detect-silence reverbed :amp 0.0001 :time 0.2 :action FREE)]
    (out 0 (pan2 (* reverbed amp amp-duck) pan))))

(def-strudel-synth saw [freq 440 detune 0 vibrato 0]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))
        f-vib (ov/vibrato:kr f-raw vibrato 0.02)
        snd (saw f-vib)]
    (rlpf snd lpf resonance)))

(def-strudel-synth sine [freq 440 detune 0 vibrato 0]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))
        f-vib (ov/vibrato:kr f-raw vibrato 0.02)
        snd (sin-osc f-vib)]
    (rlpf snd lpf resonance)))

(def-strudel-synth square [freq 440 detune 0 vibrato 0 width 0.5]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))
        f-vib (ov/vibrato:kr f-raw vibrato 0.02)
        snd (pulse f-vib width)]
    (rlpf snd lpf resonance)))

(def-strudel-synth tri [freq 440 detune 0 vibrato 0]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))
        f-vib (ov/vibrato:kr f-raw vibrato 0.02)
        snd (lf-tri f-vib)]
    (rlpf snd lpf resonance)))

(def-strudel-synth fm
  [freq 440
   detune 0
   vibrato 0
   carrier-ratio 1
   modulator-ratio 2
   mod-index 5]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))
        f-vib (ov/vibrato:kr f-raw vibrato 0.02)
        modulator (sin-osc (* f-vib modulator-ratio))
        carrier (sin-osc (+ (* f-vib carrier-ratio)
                            (* modulator mod-index f-vib)))]
    (rlpf carrier lpf resonance)))

;; --- Noise Synths ---

(def-strudel-synth white [freq 440] (rlpf (white-noise) lpf resonance))
(def-strudel-synth pink [freq 440] (rlpf (pink-noise) lpf resonance))
(def-strudel-synth brown [freq 440] (rlpf (brown-noise) lpf resonance))
(def-strudel-synth gray [freq 440] (rlpf (gray-noise) lpf resonance))
(def-strudel-synth clip [freq 440] (rlpf (clip-noise) lpf resonance))

(def-strudel-synth crackle
  [freq 440
   chaos 1.5]
  (rlpf (crackle chaos) lpf resonance))

(def-strudel-synth dust [freq 440 detune 0]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))]
    (rlpf (dust f-raw) lpf resonance)))

(def-strudel-synth dust2 [freq 440 detune 0]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))]
    (rlpf (dust2 f-raw) lpf resonance)))

(def-strudel-synth lf-noise0 [freq 440 detune 0]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))]
    (rlpf (lf-noise0 f-raw) lpf resonance)))

(def-strudel-synth lf-noise1 [freq 440 detune 0]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))]
    (rlpf (lf-noise1 f-raw) lpf resonance)))

(def-strudel-synth lf-noise2 [freq 440 detune 0]
  (let [f-raw (* freq (ov/pow 2 (/ detune 1200)))]
    (rlpf (lf-noise2 f-raw) lpf resonance)))

(def-strudel-synth tb303 [freq 440 wave 1 env-amount 1000]
  (let [freqs [freq (* 1.01 freq)]
        waves [(saw freqs)
               (pulse freqs 0.5)
               (lf-tri freqs)]
        selector (select wave waves)
        fil-env (env-gen (perc attack sustain))
        fil-lpf (s-max 20 (+ lpf (* env-amount fil-env)))]
    (rlpf selector fil-lpf (lin-lin resonance 0 1 0.9 0.05))))

(def-strudel-synth supersaw [freq 440]
  (let [input (lf-saw freq)
        shift1 (lf-saw 4)
        shift2 (lf-saw 7)
        shift3 (lf-saw 5)
        shift4 (lf-saw 2)
        comp1 (> input shift1)
        comp2 (> input shift2)
        comp3 (> input shift3)
        comp4 (> input shift4)
        output (leak-dc:ar
                 (* (- (+ (- input comp1)
                          (- input comp2)
                          (- input comp3)
                          (- input comp4)) input) 0.25))]
    (rlpf output lpf resonance)))

(def-strudel-synth mooger
  [freq 440
   osc1 0
   osc2 1
   osc1-level 0.5
   osc2-level 0.5]
  (let [osc-bank-1 [(saw freq) (sin-osc freq) (pulse freq)]
        osc-bank-2 [(saw freq) (sin-osc freq) (pulse freq)]
        s1 (* osc1-level (select osc1 osc-bank-1))
        s2 (* osc2-level (select osc2 osc-bank-2))]
    (moog-ff (+ s1 s2) lpf (lin-lin resonance 0 1 3 0))))

(def-strudel-synth ks-stringer [freq 440 coef 0.5]
  (let [noize (* 0.8 (white-noise))
        trig (impulse:kr 0)
        delay-time (/ 1.0 freq)
        plk (pluck noize trig delay-time delay-time 10 coef)]
    (rlpf plk lpf resonance)))

(defsynth dub-kick [freq 80 amp 1 sustain 0.3 lpf 2000 pan 0
                    crush 0 distort 0
                    hpf 0 bpf -1 room 0 delay 0 repeats 4
                    duck 0 duck-trigger 0 duck-attack 0.001 duck-release 0.2
                    room-size 0.5 damp 0.5]
  (let [lpf-env (perc 0.001 1 freq -20)
        ;; Trigger Sidechain
        _ (let [trig-env (env-gen (perc duck-attack duck-release) :level-scale duck-trigger)]
            (out:kr duck-bus trig-env))
        ;; Read Sidechain
        duck-env (in:kr duck-bus)
        amp-duck (ov/clip (- 1 (* duck duck-env)) 0 1)

        amp-env (perc 0.001 1 1 -8)
        osc-env (perc 0.001 1 freq -8)
        noiz (ov/lpf (white-noise) (+ (env-gen:kr lpf-env) 20))
        snd (ov/lpf (sin-osc (+ (env-gen:kr osc-env) 20)) 200)
        mixed (* (+ noiz snd) (env-gen amp-env :action NO-ACTION))
        ;; Standard chain
        filt (ov/hpf mixed (s-max 20 hpf))
        filt (select (> bpf 0) [filt (ov/bpf filt (s-max 20 bpf) 1)])
        dst (ov/distort (* filt (ov/dbamp (* distort 24))))
        crs (decimator
              dst
              (ov/lin-lin crush 0 1 44100 2000)
              (ov/lin-lin crush 0 1 24 4))
        gated crs ;; env already applied
        dly (select (> delay 0)
              [gated
               (+ gated
                 (comb-n gated 0.5
                   (s-max 0.0001 delay)
                   (* delay repeats)))])
        reverbed (free-verb dly room room-size damp)
        _ (detect-silence reverbed :amp 0.0001 :time 0.2 :action FREE)]
    (out 0 (pan2 (* reverbed amp amp-duck) pan))))

(defsynth dance-kick [freq 80 amp 1 sustain 0.3 lpf 2000 pan 0
                      crush 0 distort 0
                      hpf 0 bpf -1 room 0 delay 0 repeats 4
                      duck 0 duck-trigger 0 duck-attack 0.001 duck-release 0.2
                      room-size 0.5 damp 0.5]
  (let [env (env-gen (perc 0.001 1) :action NO-ACTION)
        ;; Trigger Sidechain
        _ (let [trig-env (env-gen (perc duck-attack duck-release) :level-scale duck-trigger)]
            (out:kr duck-bus trig-env))
        ;; Read Sidechain
        duck-env (in:kr duck-bus)
        amp-duck (ov/clip (- 1 (* duck duck-env)) 0 1)

        freq-env (env-gen (perc 0.001 0.1))
        snd (sin-osc (+ freq (* freq-env 200)))
        click (ov/lpf (white-noise) (+ 500 (* freq-env 2000)))
        mixed (+ snd (* 0.3 click))
        gated (* mixed env)
        ;; Standard chain
        filt (ov/hpf gated (s-max 20 hpf))
        filt (select (> bpf 0) [filt (ov/bpf filt (s-max 20 bpf) 1)])
        dst (ov/distort (* filt (ov/dbamp (* distort 24))))
        crs (decimator dst
              (ov/lin-lin crush 0 1 44100 2000)
              (ov/lin-lin crush 0 1 24 4))
        gated crs
        dly (select (> delay 0)
              [gated
               (+ gated
                 (comb-n gated 0.5
                   (s-max 0.0001 delay)
                   (* delay repeats)))])
        reverbed (free-verb dly room room-size damp)
        _ (detect-silence reverbed :amp 0.0001 :time 0.2 :action FREE)]
    (out 0 (pan2 (* reverbed amp amp-duck) pan))))

;; --- Pattern Engine ---

(defrecord Event [time duration params])
(defrecord Pattern [events cycles])

(defn make-pattern [events]
  (->Pattern events 1))

(defn parse-mini
  "Naively parses a collection into a sequence of events with duration.
   Returns a list of maps {:value v :start s :duration d}.
   Handles nested collections by subdividing the duration.
   Handles sets by creating simultaneous events with the same duration."
  ([tokens] (parse-mini tokens 0.0 1.0))
  ([tokens start duration]
   (let [n (count tokens)
         dur (if (pos? n) (/ duration (double n)) 0)]
     (mapcat (fn [[i v]]
               (let [s-time (+ start (* i dur))]
                 (cond
                   (set? v)
                   (map (fn [val]
                          {:value val
                           :start s-time
                           :duration dur})
                        v)

                   (and (sequential? v) (not (string? v)))
                   (parse-mini v s-time dur)

                   :else
                   [{:value v
                     :start s-time
                     :duration dur}])))
             (map-indexed vector tokens)))))

(defn with-param
  "Updates pattern events with a specific parameter."
  [pattern key value]
  (update pattern :events
          (fn [evs]
            (map (fn [e] (assoc-in e [:params key] value)) evs))))

(defn- ->name
  [v]
  (if (instance? clojure.lang.Named v) (name v) (str v)))

(defn- is-rest? [v]
  (#{"-" "_"} (->name v)))

(defn- try-parse-number [v]
  (if (string? v)
    (try (Double/parseDouble v) (catch Exception _ v))
    v))

(defn- make-event-list [pat key transform-fn]
  (let [parsed (parse-mini pat)]
    (keep (fn [p]
            (let [v (:value p)]
              (when-not (is-rest? v)
                (->Event (:start p)
                         (:duration p)
                         {key (transform-fn v)}))))
          parsed)))

(defn- combine-patterns [base-pat new-pat key]
  (let [base-events (:events base-pat)
        new-events (:events new-pat)]
    (assoc base-pat :events
           (mapcat (fn [be]
                     (let [mid (+ (:time be) (/ (:duration be) 2))
                           matches (filter (fn [ne]
                                             (let [s (:time ne)
                                                   e (+ s (:duration ne))]
                                               (and (<= s mid) (< mid e))))
                                           new-events)]
                       (if (seq matches)
                         (map
                           (fn [match]
                               (assoc-in be
                                 [:params key]
                                 (get-in match [:params key])))
                              matches)
                         [be])))
                   base-events))))

(defn set-param
  ([pattern key val] (set-param pattern key val try-parse-number))
  ([pattern key val transform-fn]
   (if (sequential? val)
     (combine-patterns
       pattern
       (make-pattern (make-event-list val key transform-fn))
       key)
     (with-param pattern key (transform-fn val)))))

(defn s
  "Creates a pattern from a sound string (mini-notation),
  or sets the sound of an existing pattern."
  ([pat]
   (make-pattern (make-event-list pat :sound ->name)))
  ([pattern sound-val]
   (set-param pattern :sound sound-val ->name)))

(defn simul
  "Creates a simultaneous collection (chord) from a sequence of values.
   Use inside a pattern vector.
   Example: (note [:c4 (simul [:e4 :g4])])"
  [vals]
  (set vals))

(defn note
  "Creates a pattern from a note string (mini-notation),
   or sets the note of an existing pattern."
  ([pat]
   (make-pattern (make-event-list pat :note identity)))
  ([pattern note-val]
   (set-param pattern :note note-val identity)))

(defn gain
  "Sets the gain (amplitude/volume) of the pattern.
   Values: 0.0 (silent) to 1.0 (default) or higher."
  [pattern val] (set-param pattern :amp val))

(defn swing
  "Sets the swing amount (shuffle feel).
   Delays every second 8th note by the specified amount (fraction of an 8th note).
   Values: 0.0 (straight) to ~0.33 (triplet feel) to 0.5 (hard swing)."
  [pattern val] (set-param pattern :swing val))

(defn duck
  "Sets the ducking amount (how much this sound is ducked by the sidechain).
   Values: 0.0 (none) to 1.0 (full duck)."
  [pattern val] (set-param pattern :duck val))

(defn duck-trigger
  "Sets the ducking trigger amount (how much this sound triggers the sidechain).
   Values: 0.0 (none) to 1.0 (full trigger)."
  [pattern val] (set-param pattern :duck-trigger val))

(defn duck-attack
  "Sets the sidechain trigger attack time in seconds.
   Default: 0.001 (1ms)."
  [pattern val] (set-param pattern :duck-attack val))

(defn duck-release
  "Sets the sidechain trigger release time in seconds.
   Default: 0.2 (200ms)."
  [pattern val] (set-param pattern :duck-release val))

(defn lpf
  "Sets the Low Pass Filter lpf frequency.
   Values: Frequency in Hz (e.g. 100 to 20000)."
  [pattern val] (set-param pattern :lpf val))

(defn pan
  "Sets the stereo panning.
   Values: -1.0 (left) to 1.0 (right). 0.0 is center."
  [pattern val] (set-param pattern :pan val))

(defn resonance
  "Sets the filter resonance (inverse bandwidth).
   Values: 0.0 (resonant) to 1.0 (flat).
   Note: In Overtone this maps to 'rq',
   so lower values mean MORE resonance."
  [pattern val] (set-param pattern :resonance val))

(defn attack
  "Sets the envelope attack time.
   Values: Time in seconds."
  [pattern val] (set-param pattern :attack val))

(defn decay
  "Sets the envelope decay time.
   Values: Time in seconds."
  [pattern val] (set-param pattern :decay val))

(defn s-level
  "Sets the envelope sustain level.
   Values: Amplitude fraction (0.0 to 1.0) relative to peak."
  [pattern val] (set-param pattern :s-level val))

(defn sustain
  "Sets the note duration (sustain time) in seconds.
   If not set, it defaults to the duration of the step."
  [pattern val] (set-param pattern :sustain val))

(defn release
  "Sets the envelope release time.
   Values: Time in seconds."
  [pattern val] (set-param pattern :release val))

(defn width
  "Sets the pulse width for square waves.
   Values: 0.0 to 1.0. 0.5 is a square wave."
  [pattern val] (set-param pattern :width val))

(defn carrier-ratio
  "Sets the FM carrier frequency ratio.
   Values: Ratio multiplier for the carrier frequency."
  [pattern val] (set-param pattern :carrier-ratio val))

(defn modulator-ratio
  "Sets the FM modulator frequency ratio.
   Values: Ratio multiplier for the modulator frequency."
  [pattern val] (set-param pattern :modulator-ratio val))

(defn mod-index
  "Sets the FM modulation index (depth).
   Values: Higher values create brighter/noisier timbres."
  [pattern val] (set-param pattern :mod-index val))

(defn detune
  "Sets the detuning amount in cents.
   Values: -100 to 100 cents (100 cents = 1 semitone)."
  [pattern val] (set-param pattern :detune val))

(defn add
  "Offsets the MIDI note number.
   Values: Semitones (e.g. 12 for +1 octave, -12 for -1 octave)."
  [pattern val] (set-param pattern :add val))

(defn chaos
  "Sets the chaos parameter for the Crackle synth.
   Values: 1.0 (steady) to 2.0 (chaotic/crackling)."
  [pattern val] (set-param pattern :chaos val))

(defn coef
  "Sets the reflection coefficient for the Karplus-Strong (ks-stringer) synth.
   Values: -1.0 to 1.0. High values result in longer decay."
  [pattern val] (set-param pattern :coef val))

(defn crush
  "Sets the bitcrushing amount.
   Values: 0.0 (clean) to 1.0 (s-max destruction: 4-bit, 2kHz sample rate)."
  [pattern val] (set-param pattern :crush val))

(defn distort
  "Sets the distortion amount.
   Values: 0.0 (clean) to 1.0 (heavy distortion)."
  [pattern val] (set-param pattern :distort val))

(defn hpf
  "Sets the High Pass Filter lpf frequency.
   Values: Frequency in Hz. 0 disables it."
  [pattern val] (set-param pattern :hpf val))

(defn bpf
  "Sets the Band Pass Filter center frequency.
   Values: Frequency in Hz. -1 disables it."
  [pattern val] (set-param pattern :bpf val))

(defn room
  "Sets the reverb mix amount (dry/wet).
   Values: 0.0 (completely dry) to 1.0 (completely wet).
   Default: 0.0 (no reverb)."
  [pattern val] (set-param pattern :room val))

(defn room-size
  "Sets the perceived size of the reverberant space.
   Values: 0.0 (small room) to 1.0 (massive hall).
   Affects decay time and reflection density.
   Default: 0.5."
  [pattern val] (set-param pattern :room-size val))

(defn damp
  "Sets the high-frequency damping of the reverb.
   Values: 0.0 (bright, reflective) to 1.0 (dark, absorbed).
   Controls how quickly high frequencies decay.
   Default: 0.5."
  [pattern val] (set-param pattern :damp val))

(defn vibrato
  "Sets the vibrato rate.
   Values: Frequency in Hz (speed). 0 disables it.
   Depth is fixed at 0.02 (2%)."
  [pattern val] (set-param pattern :vibrato val))

(defn echo-delay
  "Sets the echo delay time.
   Values: Time in seconds (e.g. 0.25). 0 disables it.
   Note: Automatically adds repeats (feedback)."
  [pattern val] (set-param pattern :delay val))

(defn echo-repeats
  "Sets the number of echo repeats (feedback).
   Values: Number of repeats (e.g. 4). Default is 4."
  [pattern val] (set-param pattern :repeats val))
(defn env
  "Sets the envelope of a pattern.
   Can be a single value or a sequence/mini-notation."
  ([pat]
   (make-pattern (make-event-list pat :env ->name)))
  ([pattern val]
   (set-param pattern :env val ->name)))

(defn active [pattern val]
  (set-param pattern :active val try-parse-number))

(defn fast [pattern amount]
  (update pattern :cycles #(* % amount)))

;; --- Player ---

(defonce metro (metronome 120))

(defn cpm
  "Sets or gets the cycles per minute.
   Assumes 4 beats per cycle."
  ([] (/ (metro-bpm metro) 4))
  ([n] (metro :bpm (* n 4)) n))

(defn fade-cpm
  "Smoothly transitions the CPM to a new value over a duration (in cycles).
   Default resolution is 1 step per cycle."
  ([target-cpm dur-cycles]
   (fade-cpm target-cpm dur-cycles 1))
  ([target-cpm dur-cycles steps-per-cycle]
   (let [dur-beats (* dur-cycles 4)
         start-cpm (cpm)
         total-steps (int (* dur-cycles steps-per-cycle))
         total-steps (max 1 total-steps)
         step-size (/ (- target-cpm start-cpm) total-steps)
         step-dur-beats (/ dur-beats total-steps)
         now (metro)]
     (dotimes [i total-steps]
       (let [beat-offset (* (inc i) step-dur-beats)
             target-val (+ start-cpm (* (inc i) step-size))]
         (apply-at (metro (+ now beat-offset))
                   (fn []
                     (tel/log! :info {:cpm target-val})
                     (cpm target-val))))))))

(defonce player-state (atom {:playing? false :patterns {} :loops #{}}))

(defn- resolve-note [n]
  (ov/midi->hz (ov/note n)))

(def ^:private synth-aliases
  {"bd" "kick"
   "sd" "snare"
   "hh" "hat"
   "cp" "clap"})

(defn- get-synth-name [sound params]
  (let [env (get params :env "adsr")]
    (str sound "-" env)))

(defn- resolve-synth [name]
  (if-let [ns (find-ns 'strudel-overtone.strudel-overtone)]
    (ns-resolve ns (symbol name))
    nil))

(defn trigger-event [ev beat dur-beats]
  (let [params (:params ev)
        active (get params :active 1)
        active? (if (number? active) (not (zero? active)) active)]
    (when active?
      (let [sound-param (:sound params)
            n (:note params)
            note-offset (get params :add 0)
            amp (let [a (or (:amp params) 1.0)]
                  (if (string? a)
                    (try (Double/parseDouble a)
                         (catch Exception _ 1.0))
                    a))
            lpf (let [c (or (:lpf params) 2000)]
                     (if (string? c)
                       (try (Double/parseDouble c)
                            (catch Exception _ 2000))
                       c))
            ;; Calculate sustain in seconds from beats, or use explicit param
            param-sustain (:sustain params)
            sustain-sec (if param-sustain
                          (if (string? param-sustain)
                            (try
                              (Double/parseDouble param-sustain)
                              (catch Exception _ 0.1))
                            param-sustain)
                          (* dur-beats (/ 60 (metro-bpm metro))))
            ;; Default sound if only note is provided
            sound-name (or sound-param (if n "saw" nil))]

        (when sound-name
          (let [base (get synth-aliases sound-name sound-name)
                synth-key (get-synth-name base params)
                synth-var (or
                           (resolve-synth synth-key)
                           (resolve-synth base))
                freq (if n
                       (resolve-note
                         (+ (if (keyword? n) (ov/note n) n) note-offset))
                       nil)
                reserved #{:sound :note :active :start :duration :env :add :swing}
                handled #{:amp :lpf :sustain :freq}
                args (cond-> (reduce-kv (fn [acc k v]
                                          (if (or (reserved k) (handled k))
                                            acc
                                            (conj acc k v)))
                                        []
                                        params)
                       true (conj :amp amp)
                       freq (conj :freq freq)
                       lpf (conj :lpf lpf)
                       sustain-sec (conj :sustain sustain-sec))]
            (when synth-var
              (do
                (apply-at (metro beat)
                          (fn [& e] (tel/log! :info {:event (into {} e)})) ev)
                (apply-at (metro beat) synth-var args)))))))))

(defn- apply-swing [t amount step-size]
  (let [step-idx (long (/ t step-size))]
    (if (odd? step-idx)
      (+ t (* amount step-size))
      t)))

(defn play-loop [key beat]
  (let [state @player-state]
    (if (and (:playing? state)
             (contains? (:loops state) key))
      (let [pat (get-in state [:patterns key])]
        (if pat
          (let [cycles (:cycles pat 1) ;; Speed multiplier
                cycle-dur (/ 4 cycles) ;; Beats per cycle (assuming 4/4)
                next-beat (+ beat cycle-dur)
                events (:events pat)
                ;; Determine grid from smallest note duration, default to 1/8 (0.125) if empty
                min-dur (if (seq events)
                          (apply min (map :duration events))
                          0.125)]

            ;; Schedule events for this cycle
            (doseq [ev events]
              (let [orig-start (:time ev)
                    swing-amount (get-in ev [:params :swing] 0)
                    swung-start (if (zero? swing-amount)
                                  orig-start
                                  (apply-swing orig-start swing-amount min-dur))
                    ;; Assoc effective start time for logging/debugging
                    ev (assoc ev :effective-time swung-start)
                    
                    rel-dur (:duration ev)
                    ev-beat (+ beat (* swung-start cycle-dur))
                    ev-dur-beats (* rel-dur cycle-dur)]
                (trigger-event ev ev-beat ev-dur-beats)))

            (apply-by (metro next-beat) #'play-loop [key next-beat]))
          ;; Pattern removed, loop dies
          (swap! player-state update :loops disj key)))
      ;; Stopped, loop dies
      (swap! player-state update :loops disj key))))

(defn play!
  [& args]
  (let [pairs (if (= 1 (count args))
                [[:main (first args)]]
                (partition 2 args))]
    (doseq [[key pattern] pairs]
      (let [start-loop? (not (contains? (:loops @player-state) key))]
        (swap! player-state (fn [s]
                              (-> s
                                  (assoc :playing? true)
                                  (assoc-in [:patterns key] pattern)
                                  (update :loops conj key))))
        (when start-loop?
          (let [now (metro)
                start-beat (+ now (- 4 (mod now 4)))]
            (apply-by (metro start-beat) #'play-loop [key start-beat])))))))

(defn stop!
  ([] (swap! player-state assoc :playing? false :patterns {} :loops #{}))
  ([key] (swap! player-state update :patterns dissoc key)))

;; --- Main / Entry ---

(defn -main [& args]
  (connect-server)
  (println "Strudel-Overtone Ready."))

(comment

  (connect-server)

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
          (lpf 1000)))

  (play! :bd
         (->
          (s [:bd :_ :_ :_ :bd :_])
          (fast 2)
          (lpf 500)))

  (play! :sd
         (->
          (s [:_ :_ :_ :sd :_ :_ :_])
          (fast 2)
          (gain 0.25)
          (lpf 5000)))

  ;; Update bassline
  (play! :bass (-> (note [:c2 :_ :b2 :_]) (s :sine) (gain 1)))

  (play! :bass (-> (note [:c2])
                   (s [:sine :tri])))

  (play! :arp
         (->
          (note [:c4 :_ :d4 :_ :e4 :_ :f4 :_ :g4 :_ :f4 :_ :e4 :_ :d4 :_])
          (s :sine)
          (fast 2)
          (gain 1)
          (lpf 100)))

  ;; --- New Synths ---

  (play! :hh
         (-> (s [:hh :hh :hh :hh])
             (fast 1)
             (gain 0.3)))

  (play! :cp
         (-> (s [:_ :_ :cp :_])
             (fast 2)
             (gain 0.5)))

  (play! :lead
         (-> (note [:c3 :e3 :g3 :b3])
             (s :square)
             (fast 2)
             (lpf 1200)))

  (play! :soft
         (-> (note [:f4 :a4 :c5 :b4])
             (s :tri)
             (fast 0.5)
             (gain 0.8)))

  (play! :metal
         (->
          (note [:g2 :f2 :g2 :g2 :g2])
          (gain (concat (range 0.0 1.0 0.05) (range 1.0 0.0 -0.05)))
          (lpf (map
                 (partial * 1000)
                 (concat
                   (range 0.0 1.0 0.05)
                   (range 1.0 0.0 -0.05))))
          (s :fm)
          (fast 1)))

  (stop!)

  (cpm (/ 140 4))

  (cpm)

  (play!
   :bd (-> (s [:bd :bd :bd :bd]))
   :sd (-> (s [:- :- :sd :-]))
   :bass (-> (note [:c2 :b2]) (s :sine) (fast 0.5)))

  (play!
   :bd (-> (s [:bd :bd :- :- :- :- :- :-]) (note [:a1 :c2]))
   :bass (-> (s [:bd :bd :- :- :- :- :- :-]) (note [:a1 :c2])))

  (play!
   :arp (->
         (note (->> (chord :c4 :minor7) chosen-from (take 16)))
         (s :sine)
         (gain (take 16 (chosen-from (map (partial * 1/16) (range 16)))))
         (active [0]))

   :bass (->
          (note (->> (chord :c1 :minor7) chosen-from (take 4)))
          (s :square)
          (lpf 400)
          (fast 1/8)
          (gain [0.2]
            #_(take 4 (chosen-from (map (partial * 1/16) (range 16))))))

   :bd (->
        (s [:bd :bd :- :-])
        (note [:a1])
        (fast 1))
   :hh (->
        (s [:hh :hh :hh :hh])
        (fast 2)
        (gain 0.1)
        (active [0]))

   :sd (->
        (s [:sd :sd :- :sd])
        (fast 4)
        (gain 0.5)))

  (stop!)

;; Stop just the drums
  (stop! :drums)
  (stop! :snare)

  (stop! :bd)

  (stop! :bass)

  (stop! :arp)

  ;; Stop everything
  (stop!)

  (->> [1 2 3] shuffle (take 2))

  (take 16 (chosen-from (chord :c4 :minor7)))

  (cpm (/ 80 4))

  (play!
   :bd (->
        (s [:bd :bd :bd :bd :bd :bd :bd [:bd :bd]])
        (gain 1)
        (active 0 #_(chosen-from [0 0 1] 8)))
   :bd-4 (->
          (s [:bd :bd :bd :bd])
          (pan 0)
          (gain 1)
          (active 0))
   :sd (->
        (s (take 16 (cycle [:- :sd])))
        (gain 0.2)
        (active 0))
   :clap (->
          (s (cons :fm (repeat 7 :-)))
          (env :perc)
          (gain 0.5)
          (echo-repeats 50)
          (room 0.5)
          (active 0))
   :bass-1 (->
            (note [:c1 :bb0])
            (s [:fm])
            (carrier-ratio 2)
            (modulator-ratio 3)
            (release 0)
            (gain 0.1)
            (active 0))
   :bass-2 (->
            (s [:tri :- :tri :-])
            (note (shuffle [:a1 :c2]))
            (attack 2)
            (decay 2)
            (gain 0.1)
            (active 0))
   :bass-3 (->
            (s [:- :- :saw :- :- :saw])
            (note (shuffle [:g2 :b3]))
            (attack 2)
            (decay 1)
            (gain 0.1)
            (active 0))
   :arp (->
         (note [(set (chord :a3 :major)) (set (chord :a3 :minor))])
         (s [#{:sine :tri}])
         (gain 0.2)
         (env [#{:adsr :perc}])
         (fast 1)))

  (stop!)

  (cpm (/ 174 4))

  (play!
   :kick (->
          (s [[:kick] :_ [:- :kick] :_])
          #_(s [:kick :- :kick :-])
          (note :d1)
          (env :perc)
          (room 0.5)
          (distort 0.5)
          (gain 0.35)
          (duck 1)
          (lpf (chosen-from [100 200 400 800] 2))
          #_(active 0))
   :snare (->
           (s [:- :- [:snare :- :-] :- :- :- [:snare :-] :-])
           (lpf 10000)
           (gain 0.4)
           (duck 1)
           (active (chosen-from [1 1 1 1] 2))
           #_(active 0))
   :hat   (->
           (s (repeat 8 :hat))
           (gain 0.05)
           (active (chosen-from [0 1 1] 8))
           (duck 1)
           #_(active 0))
   :shaker   (->
              (s (repeat 16 :clap))
              (gain 0.20)
              (active (chosen-from [0 1 1] 4))
              (duck 1)
              #_(active 0))
   :bass (->
          (note [(set (take 3 (chord :f0 :minor)))
                 (set (take 3 (chord :bb0 :minor)))
                 (set (take 3 (chord :c0 :major)))
                 (set (take 3 (chord :d0 :major)))])
          (s [:supersaw])
          (vibrato 1)
          (attack 0.5)
          (gain [0.6 0.7])
          (pan (chosen-from (range -0.5 0.5 0.1) 4))
          (fast 1/4)
          (duck-trigger 1)
          (active 0))
   :lead (->
          (note (chosen-from (take 5 (scale :f4 :major)) 32))
          (s [:supersaw])
          (env (chosen-from [:perc :adsr] 8))
          (gain 0.1)
          (pan (chosen-from (range -0.75 0.75 0.05) 4))
          (fast 1/4)
          (duck-trigger 1)
          (active (chosen-from [0 0 1] 8))
          #_(active 0)))

  (stop!)

  ;; --- New Overtone Synths Examples ---

  (play!
   :acid (->
          (note [:c2 :c3 :bb2 :g2])
          (s :tb303)
          (lpf [500 1000 2000 500])
          (resonance 0.2)
          (fast 2)
          (gain 0.5))

   :trance (->
            (note [:c4 :g4 :c5 :g5])
            (s :supersaw)
            (detune 10)
            (sustain 0.5)
            (room 0.5)
            (gain 0.4)
            (fast 0.5))

   :moog (->
          (note [:c2 :_ :c2 :_])
          (s :mooger)
          (lpf 800)
          (resonance 0.1)
          (gain 0.6))

   :plucks (->
            (note (scale :c4 :minor))
            (s :ks-stringer)
            (coef 0.8)
            (fast 2)
            (gain 0.6))

   :dub (->
         (s [:dub-kick :_ :_ :_])
         (gain 0.8))

   :dance (->
           (s [:dance-kick :dance-kick :dance-kick :dance-kick])
           (fast 2)
           (gain 0.8))

   )

  (play!

   :plucks (->
             (note (chosen-from (chord :c4 :minor) 4))
            (s :tb303)
            (env :perc)
            (fast 1)
            (swing [0.3])
            (gain 0.5))
   )

  (cpm)

  (fade-cpm 60 8 2)

  (stop!)

  (play!
    :kick (->
            (s [#{[:kick :- :-] :dub-kick} :-
                [:- #{[:kick :- :-] :dub-kick}] :-])
            (note [:d2 :c2])
            (gain 1)
            (duck-trigger 1)
            )
    :snare (->
             (s [:- :snare :- :snare])
             (gain [0.6 0.8])
             (swing [0.5 0.0])
             (duck-trigger 1)
             (note [:e3 :c3]))
    :hat (->
           (s (take 8 (cycle [:hat])))
           (gain 0.5)
           (duck 0.9))
    :pad (->
           (note [:c5 [:b4 :b4 :c5]])
           (add [-24])
           (attack 0.2)
           (release 1)
           (s [#{:mooger}])
           (gain 0.5)
           (s-level 1)
           (duck 0.8)))

  (stop!)


  .)

