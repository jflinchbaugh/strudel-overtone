(ns strudel-overtone.pattern
  (:require [overtone.core :as ov]))

;; --- Data Structures ---

(defrecord Event [time duration params])
(defrecord Pattern [events cycles delay-cycles stop-cycles length])
(defrecord Overlay [val])

(def ^:dynamic *current-cycle* 0)
(def ^:dynamic *current-event* nil)

(defn overlay
  "Wraps a parameter value (like a pattern vector) to indicate it should
   be applied as an overlay, without splitting existing events."
  [val]
  (->Overlay val))

(defn make-pattern [events]
  (let [num-cycles (if (seq events)
                     (inc (long (apply max (map :time events))))
                     1)]
    (->Pattern events (constantly 1) 0 nil num-cycles)))

;; --- Randomness ---

(defonce global-seed (atom 0))

(defn seed!
  "Sets the global seed for repeatable randomness.
   Default is 0."
  [n]
  (reset! global-seed n))

(defn repeatable-rand
  "Returns a deterministic random value [0, 1) based on time t,
   a seed-offset (usually the param key), and the global-seed."
  [t seed-offset]
  (let [s @global-seed
        h (hash [s t seed-offset])]
    (/ (Math/abs h) (double Integer/MAX_VALUE))))

(defn irand
  "Returns a repeatable random integer stream between low (inclusive)
   and high (inclusive). If only one arg is provided, it's [0, high]."
  ([high] (irand 0 high))
  ([low high]
   (fn [t seed]
     (let [r (repeatable-rand t seed)]
       (int (+ low (* r (- high low -1))))))))

(defn srand
  "Returns a repeatable random float stream between low and high.
   Defaults to [0, 1)."
  ([] (srand 0 1))
  ([high] (srand 0 high))
  ([low high]
   (fn [t seed]
     (let [r (repeatable-rand t seed)]
       (+ low (* r (- high low)))))))

(defn choose
  "Returns a repeatable random stream that picks an element from coll."
  [coll]
  (fn [t seed]
    (let [r (repeatable-rand t seed)
          idx (int (* r (count coll)))]
      (nth coll idx))))

(defn wchoose
  "Returns a repeatable random stream that picks from choices based on weights.
   pairs: a collection of [value weight] pairs."
  [pairs]
  (let [choices (mapv first pairs)
        weights (mapv second pairs)
        total (double (reduce + weights))
        cum-weights (vec (reductions + weights))]
    (fn [t seed]
      (if (zero? total)
        (rand-nth choices)
        (let [r (* (repeatable-rand t seed) total)]
          (loop [i 0]
            (if (< r (cum-weights i))
              (choices i)
              (if (< (inc i) (count cum-weights))
                (recur (inc i))
                (choices (dec (count choices)))))))))))

(defn- chosen-from
  "Returns an infinite sequence of repeatable random stream functions
   that each pick from coll. Useful for initializing patterns.
   Example: (note (choose-n 16 [:c2 :e2 :g2]))"
  [coll]
  (map (fn [i]
         (fn [t seed]
           (let [r (repeatable-rand (+ t (* i 0.001)) seed)]
             (nth coll (int (* r (count coll)))))))
       (range)))

(defn choose-n
  "Returns a sequence of n repeatable random stream functions
   that each pick from coll. Useful for creating random sequences.
   Example: (s (choose-n 4 [\"kick\" \"snare\" \"hat\"]))"
  [n coll]
  (take n (chosen-from coll)))

(defn rtake
  "Manually get some values from a random stream function.
  Get n values for id usage from rand-f."
  [n id rand-f]
  (for [t (range n)] (rand-f t id)))

;; --- Signals ---

(defn- tau [t] (* 2 Math/PI t))

(defn sig-range
  "Wraps a signal function to scale its output to [low, high]."
  [sig low high]
  (fn [t seed]
    (let [v (sig t seed)]
      (+ low (* v (- high low))))))

(defn sine-sig
  "Returns a continuous sine wave signal (0 to 1).
   Multi-arity:
   [] -> function [t _] returning sine at freq 1
   [t _] -> value at time t
   [freq] -> function [t _] returning sine at freq
   [freq low high] -> function [t _] returning sine at freq scaled to [low, high]"
  ([] (sine-sig 1))
  ([t _]
   (+ 0.5 (* 0.5 (Math/sin (tau t)))))
  ([freq]
   (fn [t _]
     (sine-sig (* t freq) nil)))
  ([freq low high]
   (sig-range (sine-sig freq) low high)))

(defn saw-sig
  "Returns a continuous sawtooth wave signal (0 to 1).
   Multi-arity:
   [] -> function [t _] returning saw at freq 1
   [t _] -> value at time t
   [freq] -> function [t _] returning saw at freq
   [freq low high] -> function [t _] returning saw at freq scaled to [low, high]"
  ([] (saw-sig 1))
  ([t _]
   (mod t 1))
  ([freq]
   (fn [t _]
     (saw-sig (* t freq) nil)))
  ([freq low high]
   (sig-range (saw-sig freq) low high)))

(defn tri-sig
  "Returns a continuous triangle wave signal (0 to 1).
   Multi-arity:
   [] -> function [t _] returning tri at freq 1
   [t _] -> value at time t
   [freq] -> function [t _] returning tri at freq
   [freq low high] -> function [t _] returning tri at freq scaled to [low, high]"
  ([] (tri-sig 1))
  ([t _]
   (let [x (mod t 1)]
     (if (< x 0.5)
       (* 2 x)
       (- 2 (* 2 x)))))
  ([freq]
   (fn [t _]
     (tri-sig (* t freq) nil)))
  ([freq low high]
   (sig-range (tri-sig freq) low high)))

(defn square-sig
  "Returns a continuous square wave signal (0 to 1).
   Multi-arity:
   [] -> function [t _] returning square at freq 1
   [t _] -> value at time t
   [freq] -> function [t _] returning square at freq
   [freq low high] -> function [t _] returning square at freq scaled to [low, high]"
  ([] (square-sig 1))
  ([t _]
   (if (< (mod t 1) 0.5) 1 0))
  ([freq]
   (fn [t _]
     (square-sig (* t freq) nil)))
  ([freq low high]
   (sig-range (square-sig freq) low high)))

(defn cosine-sig
  "Returns a continuous cosine wave signal (0 to 1).
   Multi-arity:
   [] -> function [t _] returning cosine at freq 1
   [t _] -> value at time t
   [freq] -> function [t _] returning cosine at freq
   [freq low high] -> function [t _] returning cosine at freq scaled to [low, high]"
  ([] (cosine-sig 1))
  ([t _]
   (+ 0.5 (* 0.5 (Math/cos (tau t)))))
  ([freq]
   (fn [t _]
     (cosine-sig (* t freq) nil)))
  ([freq low high]
   (sig-range (cosine-sig freq) low high)))

(defn adsr-sig
  "Generates an ADSR envelope function over step duration (0 to 1).
   Multi-arity:
   [attack decay sustain-level release] -> envelope value [0.0, 1.0]
   [attack decay sustain-level release min-val max-val] -> scaled envelope value [min-val, max-val]"
  ([attack decay sustain-level release]
   (fn [t _]
     (let [rel-t (mod t 1)
           env-val (cond
                     (< rel-t attack)
                     (/ rel-t (max 0.001 attack))

                     (< rel-t (+ attack decay))
                     (let [decay-t (/ (- rel-t attack) (max 0.001 decay))]
                       (- 1.0 (* (- 1.0 sustain-level) decay-t)))

                     (< rel-t (- 1.0 release))
                     sustain-level

                     :else
                     (let [rel-phase (/ (- rel-t (- 1.0 release)) (max 0.001 release))]
                       (* sustain-level (- 1.0 rel-phase))))]
       (double env-val))))
  ([attack decay sustain-level release min-val max-val]
   (sig-range (adsr-sig attack decay sustain-level release) min-val max-val)))

;; --- Mini-notation Parser ---

(defn parse-mini
  "Naively parses a collection or single value into a sequence of events.
   Returns a list of maps {:value v :start s :duration d}.
   Handles nested collections by subdividing the duration.
   Handles sets by creating simultaneous events with the same duration."
  ([tokens] (parse-mini tokens 0.0 1.0))
  ([tokens start duration]
   (if (sequential? tokens)
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
               (map-indexed vector tokens)))
     [{:value tokens
       :start start
       :duration duration}])))

;; --- Core Pattern Builders ---

(defn ->name
  [v]
  (cond
    (fn? v) v
    (instance? clojure.lang.Named v) (keyword (name v))
    :else (keyword (str v))))

(defn is-rest? [v]
  (#{:- :_} (->name v)))

(defn- try-parse-number [v]
  (cond
    (fn? v) v
    (string? v) (try (Double/parseDouble v) (catch Exception _ v))
    :else v))

(defn- wrap-number-fn [v]
  (if (number? v) (constantly v) v))

(defn- is-rest-params? [params]
  (let [v (get params :active (constantly 1))]
    (cond
      (fn? v) (try (zero? (v 0 :active)) (catch Exception _ false))
      (number? v) (zero? v)
      :else false)))

(defn with-param
  "Updates pattern events with a specific parameter.
   Only applies to active events (not rests)."
  [pattern key param-value]
  (update pattern :events
          (fn [evs]
            (map (fn [e]
                   (if (is-rest-params? (:params e))
                     e
                     (let [v (wrap-number-fn param-value)]
                       (assoc-in e [:params key]
                                 (if (and (= key :note) (contains? (:params e) :note))
                                   ;; Special case for :note to allow composition
                                   (let [old-v (get-in e [:params :note])]
                                     (fn [beat k]
                                       (let [root (if (fn? old-v) (old-v beat k) old-v)]
                                         (if (fn? v) (v beat k) v))))
                                   v)))))
                 evs))))

(defn- make-event-list [pat key transform-fn]
  (let [parsed (parse-mini pat)]
    (map (fn [p]
           (let [v (:value p)]
             (if (is-rest? v)
               (->Event (:start p)
                        (:duration p)
                        {:active (constantly 0)})
               (let [res (wrap-number-fn (transform-fn v))]
                 (->Event (:start p)
                          (:duration p)
                          {key res})))))
         parsed)))

(defn- combine-patterns
  ([base-pat new-pat key] (combine-patterns base-pat new-pat key false))
  ([base-pat new-pat key overlay?]
   (let [base-events (:events base-pat)
         new-events (:events new-pat)
         new-len (or (:length new-pat) 1.0)
         max-base-t (if (seq base-events)
                      (apply max (map (fn [e] (+ (:time e) (:duration e))) base-events))
                      0)
         ;; Unroll new-events to cover the range of base-events
         unroll-count (long (Math/ceil (/ max-base-t (double new-len))))
         unrolled-new (if (> unroll-count 1)
                        (mapcat (fn [i]
                                  (map (fn [ev]
                                         (update ev :time + (* i new-len)))
                                       new-events))
                                (range unroll-count))
                        new-events)]
     (assoc base-pat :events
            (if overlay?
              (map (fn [be]
                     (let [b-start (:time be)
                           match (first (filter (fn [ne]
                                                  (let [n-start (:time ne)
                                                        n-end (+ n-start (:duration ne))]
                                                    (and (<= n-start b-start) (< b-start n-end))))
                                                unrolled-new))]
                       (if match
                         (let [merged-params (merge (:params be) (:params match))]
                           (assoc be :params
                                  (if (or (is-rest-params? (:params be))
                                          (is-rest-params? (:params match)))
                                    (assoc merged-params :active (constantly 0))
                                    merged-params)))
                         be)))
                   base-events)
              (mapcat (fn [be]
                        (let [b-start (:time be)
                              b-end (+ b-start (:duration be))
                              matches (filter (fn [ne]
                                                (let [n-start (:time ne)
                                                      n-end (+ n-start (:duration ne))]
                                                  (and (< n-start b-end) (> n-end b-start))))
                                              unrolled-new)]
                          (if (seq matches)
                            (map (fn [match]
                                   (let [n-start (:time match)
                                         n-end (+ n-start (:duration match))
                                         i-start (max b-start n-start)
                                         i-end (min b-end n-end)
                                         i-dur (- i-end i-start)
                                         merged-params (merge (:params be) (:params match))]
                                     (assoc be
                                            :time i-start
                                            :duration i-dur
                                            :params (if (or (is-rest-params? (:params be))
                                                            (is-rest-params? (:params match)))
                                                      (assoc merged-params :active (constantly 0))
                                                      merged-params))))
                                 matches)
                            [be])))
                      base-events))))))

(defn set-param
  ([pattern key param-value] (set-param pattern key param-value try-parse-number))
  ([pattern key param-value transform-fn]
   (let [overlay? (instance? Overlay param-value)
         v (if overlay? (:val param-value) param-value)]
     (if (sequential? v)
       (combine-patterns
        pattern
        (make-pattern (make-event-list v key (comp wrap-number-fn transform-fn)))
        key
        overlay?)
       (with-param pattern key (wrap-number-fn (transform-fn v)))))))

(defn params
  "Sets multiple parameters at once from a map.
   Example: (params pat {:attack 0.1 :release 0.5})"
  [pattern param-map]
  (reduce-kv (fn [p k v] (set-param p k v)) pattern param-map))

(defn alt
  "Returns a function that alternates between values every cycle.
   Use inside a pattern vector.
   Example: (s [:bd (alt :sd :cp) :bd :hh])"
  [& coll]
  (fn [_ _]
    (let [cycle *current-cycle*
          idx (mod (long cycle) (count coll))]
      (nth coll idx))))

(defn slowcat
  "Concatenates multiple patterns in time.
   Each pattern's length is respected (defaulting to 1 cycle).
   Example: (slowcat (s [:bd :sd]) (s [:hh :hh]))"
  [& patterns]
  (let [pats (filter some? patterns)]
    (if (empty? pats)
      (make-pattern [])
      (let [total-len (reduce + (map (fn [p] (or (:length p) 1.0)) pats))
            new-events (mapcat (fn [[idx pat]]
                                 (let [ps (take idx pats)
                                       offset (reduce + (map #(or (:length %) 1.0) ps))]
                                   (map (fn [ev] (update ev :time + offset))
                                        (:events pat))))
                               (map-indexed vector pats))]
        (assoc (first pats)
               :events new-events
               :length total-len)))))

(defn stack
  "Layers multiple patterns to play simultaneously.
   The resulting length is the maximum length of all patterns."
  [& patterns]
  (let [pats (filter some? patterns)]
    (if (empty? pats)
      (make-pattern [])
      (let [max-len (apply max (map (fn [p] (or (:length p) 1.0)) pats))]
        (assoc (first pats)
               :events (mapcat :events pats)
               :length max-len)))))

(defn fastcat
  "Squeezes multiple patterns into a single cycle.
   Each pattern is scaled to take 1/n of the cycle."
  [& patterns]
  (let [pats (filter some? patterns)
        n (count pats)]
    (if (zero? n)
      (make-pattern [])
      (let [new-events (mapcat (fn [[idx pat]]
                                 (let [offset (/ (double idx) n)
                                       scale (/ 1.0 n)]
                                   (map (fn [ev]
                                          (-> ev
                                              (update :time #(+ offset (* % scale)))
                                              (update :duration * scale)))
                                        (:events pat))))
                               (map-indexed vector pats))]
        (assoc (first pats) :events new-events :length 1.0)))))

;; --- DSL Modifiers ---

(defn s
  "Creates a pattern from a sound string (mini-notation),
  or sets the sound of an existing pattern."
  ([pat]
   (make-pattern (make-event-list pat :sound ->name)))
  ([pattern sound-name]
   (set-param pattern :sound sound-name ->name)))

(defn simul
  "Creates a simultaneous collection (chord) from a sequence of values.
   Use inside a pattern vector.
   Example: (note [:c4 (simul [:e4 :g4])])"
  [pattern]
  (set pattern))

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
  [pattern gain-amp] (set-param pattern :amp gain-amp))

(defn swing
  "Sets the swing amount (shuffle feel).
   Delays every second 8th note by the specified amount (fraction of an 8th note).
   Values: 0.0 (straight) to ~0.33 (triplet feel) to 0.5 (hard swing)."
  [pattern swing-amount] (set-param pattern :swing swing-amount))

(defn duck
  "Sets the ducking amount (how much this sound is ducked by the sidechain).
   Values: 0.0 (none) to 1.0 (full duck)."
  [pattern duck-amount] (set-param pattern :duck duck-amount))

(defn duck-trigger
  "Sets the ducking trigger amount (how much this sound triggers the sidechain).
   Values: 0.0 (none) to 1.0 (full trigger)."
  [pattern trigger-amount] (set-param pattern :duck-trigger trigger-amount))

(defn duck-attack
  "Sets the sidechain trigger attack time in seconds.
   Default: 0.001 (1ms)."
  [pattern attack-sec] (set-param pattern :duck-attack attack-sec))

(defn duck-release
  "Sets the sidechain trigger release time in seconds.
   Default: 0.2 (200ms)."
  [pattern release-sec] (set-param pattern :duck-release release-sec))

(defn lpf
  "Sets the Low Pass Filter lpf frequency.
   Values: Frequency in Hz (e.g. 100 to 20000)."
  [pattern cutoff-hz] (set-param pattern :lpf cutoff-hz))

(defn pan
  "Sets the stereo panning.
   Values: -1.0 (left) to 1.0 (right). 0.0 is center."
  [pattern pan-pos] (set-param pattern :pan pan-pos))

(defn resonance
  "Sets the filter resonance (inverse bandwidth).
   Values: 0.0 (resonant) to 1.0 (flat).
   Note: In Overtone this maps to 'rq',
   so lower values mean MORE resonance."
  [pattern res-amount] (set-param pattern :resonance res-amount))

(defn sustain
  "Sets the note duration (sustain time) in seconds.
   If not set, it defaults to the duration of the step."
  [pattern sustain-sec] (set-param pattern :sustain sustain-sec))

(defn legato
  "Sets the note legato (duration multiplier).
   Values: 1.0 (standard), >1.0 (overlapping), <1.0 (staccato)."
  [pattern legato-amount] (set-param pattern :legato legato-amount))

(defn monophonic
  "Enables monophonic mode for the pattern.
   In monophonic mode, a single synth instance is used and its parameters
   (like frequency) are updated for each note, rather than triggering
   a new synth instance."
  ([pattern] (set-param pattern :monophonic 1))
  ([pattern monophonic-active] (set-param pattern :monophonic monophonic-active)))

(def mono monophonic)

(defn width
  "Sets the pulse width for square waves.
   Values: 0.0 to 1.0. 0.5 is a square wave."
  [pattern width-amount] (set-param pattern :width width-amount))

(defn carrier-ratio
  "Sets the FM carrier frequency ratio.
   Values: Ratio multiplier for the carrier frequency."
  [pattern ratio] (set-param pattern :carrier-ratio ratio))

(defn modulator-ratio
  "Sets the FM modulator frequency ratio.
   Values: Ratio multiplier for the modulator frequency."
  [pattern ratio] (set-param pattern :modulator-ratio ratio))

(defn mod-index
  "Sets the FM modulation index (depth).
   Values: Higher values create brighter/noisier timbres."
  [pattern index-amount] (set-param pattern :mod-index index-amount))

(defn detune
  "Sets the detuning amount in cents.
   Values: -100 to 100 cents (100 cents = 1 semitone)."
  [pattern detune-cents] (set-param pattern :detune detune-cents))

(defn add
  "Offsets the MIDI note number.
   Values: Semitones (e.g. 12 for +1 octave, -12 for -1 octave)."
  [pattern note-offset] (set-param pattern :add note-offset))

(defn degrees
  "Maps degree integers to MIDI notes using a scale.
   degree-vals: a list pattern of integers (1-indexed).
   Example: (-> (note :c4) (degrees :major [1 3 5 8]))"
  [pattern scale-name degree-vals]
  (set-param pattern :degree degree-vals
             (fn [d]
               (fn [beat key]
                 (ov/degree->interval d scale-name)))))

(defn chaos
  "Sets the chaos parameter for the Crackle synth.
   Values: 1.0 (steady) to 2.0 (chaotic/crackling)."
  [pattern chaos-amount] (set-param pattern :chaos chaos-amount))

(defn coef
  "Sets the reflection coefficient for the Karplus-Strong (ks-stringer) synth.
   Values: -1.0 to 1.0. High values result in longer decay."
  [pattern reflection-coef] (set-param pattern :coef reflection-coef))

(defn crush
  "Sets the bitcrushing amount.
   Values: 0.0 (clean) to 1.0 (s-max destruction: 4-bit, 2kHz sample rate)."
  [pattern crush-amount] (set-param pattern :crush crush-amount))

(defn distort
  "Sets the distortion amount.
   Values: 0.0 (clean) to 1.0 (heavy distortion)."
  [pattern distort-amount] (set-param pattern :distort distort-amount))

(defn hpf
  "Sets the High Pass Filter lpf frequency.
   Values: Frequency in Hz. 0 disables it."
  [pattern cutoff-hz] (set-param pattern :hpf cutoff-hz))

(defn bpf
  "Sets the Band Pass Filter center frequency.
   Values: Frequency in Hz. -1 disables it."
  [pattern cutoff-hz] (set-param pattern :bpf cutoff-hz))

(defn room
  "Sets the reverb mix amount (dry/wet).
   Values: 0.0 (completely dry) to 1.0 (completely wet).
   Default: 0.0 (no reverb)."
  [pattern room-amount] (set-param pattern :room room-amount))

(defn room-size
  "Sets the perceived size of the reverberant space.
   Values: 0.0 (small room) to 1.0 (massive hall).
   Affects decay time and reflection density.
   Default: 0.5."
  [pattern size-amount] (set-param pattern :room-size size-amount))

(defn damp
  "Sets the high-frequency damping of the reverb.
   Values: 0.0 (bright, reflective) to 1.0 (dark, absorbed).
   Controls how quickly high frequencies decay.
   Default: 0.5."
  [pattern damp-amount] (set-param pattern :damp damp-amount))

(defn vibrato
  "Sets the vibrato rate.
   Values: Frequency in Hz (speed). 0 disables it.
   Depth is fixed at 0.02 (2%)."
  [pattern freq-hz] (set-param pattern :vibrato freq-hz))

(defn echo-delay
  "Sets the echo delay time.
   Values: Time in seconds (e.g. 0.25). 0 disables it.
   Note: Automatically adds repeats (feedback)."
  [pattern delay-sec] (set-param pattern :delay delay-sec))

(defn echo-repeats
  "Sets the number of echo repeats (feedback).
   Values: Number of repeats (e.g. 4). Default is 4."
  [pattern num-repeats] (set-param pattern :repeats num-repeats))

(defn step
  "Sets the step parameter, primarily used by additive synths.
   Determines the spacing between harmonics."
  [pattern step-size] (set-param pattern :step step-size))

(defn rate
  "Sets the playback rate for samples.
   Values: 1.0 (normal), 0.5 (half speed), -1.0 (reverse), etc."
  [pattern playback-rate] (set-param pattern :rate playback-rate))

(def speed
  "Alias for rate.
   Sets the playback rate for samples."
  rate)

(defn pshift
  "Sets the pitch shift in semitones.
   Uses a time-domain granular pitch shifter.
   Values: -24 to 24 (default 0)."
  [pattern shift-semitones] (set-param pattern :pshift shift-semitones))

(defn fshift
  "Sets the frequency shift in Hz.
   Shifts all frequencies by a fixed amount.
   Values: -2000 to 2000 (default 0)."
  [pattern shift-hz] (set-param pattern :fshift shift-hz))

(defn tremolo-hz
  "Sets the tremolo (amplitude modulation) frequency in Hz.
   Values: 0.1 to 20 (default 0)."
  [pattern freq-hz] (set-param pattern :tremolo-hz freq-hz))

(defn tremolo-depth
  "Sets the tremolo depth.
   Values: 0.0 (none) to 1.0 (full modulation)."
  [pattern depth-amount] (set-param pattern :tremolo-depth depth-amount))

(defn pan-hz
  "Sets the auto-pan (panning modulation) frequency in Hz.
   Values: 0.1 to 20 (default 0)."
  [pattern freq-hz] (set-param pattern :pan-hz freq-hz))

(defn pan-depth
  "Sets the auto-pan depth.
   Values: 0.0 (center) to 1.0 (full stereo sweep)."
  [pattern depth-amount] (set-param pattern :pan-depth depth-amount))

(defn phaser-hz
  "Sets the phaser frequency in Hz.
   Values: 0.1 to 10 (default 0)."
  [pattern freq-hz] (set-param pattern :phaser-hz freq-hz))

(defn phaser-depth
  "Sets the phaser depth (mix).
   Values: 0.0 (dry) to 1.0 (wet)."
  [pattern depth-amount] (set-param pattern :phaser-depth depth-amount))

(defn begin
  "Sets the start position of the sample (0.0 to 1.0)."
  [pattern pos-amount] (set-param pattern :begin pos-amount))

(defn end
  "Sets the end position of the sample (0.0 to 1.0)."
  [pattern pos-amount] (set-param pattern :end pos-amount))

(defn looping
  "Sets the loop flag. 1 for loop, 0 for one-shot. (default 0)."
  [pattern loop-active] (set-param pattern :loop? loop-active))

(defn env
  "Sets the envelope of a pattern.
   Can be a single value or a sequence/mini-notation."
  ([pat]
   (make-pattern (make-event-list pat :env ->name)))
  ([pattern env-name]
   (set-param pattern :env env-name ->name)))

(defn active [pattern active-val]
  (set-param pattern :active active-val try-parse-number))

(defn glide
  "Adds a frequency slide (glide/portamento) to the pattern.
   n: glide time in cycles (relative to cycle duration)"
  [pattern n]
  (set-param pattern :slide n))

;; --- Grouping Helpers ---

(defn adsr
  "Sets all ADSR envelope parameters at once.
   Usage: (adsr pat attack decay sustain-level release)"
  [pattern attack delay sustain-level release]
  (params pattern {:env :adsr
                   :attack attack
                   :decay delay
                   :s-level sustain-level
                   :release release}))

(defn perc
  "Sets percussive envelope parameters at once.
   Usage: (perc pat attack sustain)"
  [pattern attack-sec sustain-sec]
  (params pattern {:env :perc
                   :attack attack-sec
                   :sustain sustain-sec}))

(defn lpf-adsr
  "Sets all filter ADSR envelope parameters at once.
   Usage: (lpf-adsr pat attack decay sustain-level release)"
  [pattern attack decay sustain-level release]
  (params pattern {:lpf-env-type :adsr
                   :lpf-attack attack
                   :lpf-decay decay
                   :lpf-s-level sustain-level
                   :lpf-release release}))

(defn lpf-perc
  "Sets LPF percussive envelope parameters at once.
   Usage: (lpf-perc pat attack-sec)"
  [pattern attack-sec]
  (params pattern {:lpf-env-type :perc
                   :lpf-attack attack-sec}))

(defn hpf-adsr
  "Sets all HPF ADSR envelope parameters at once.
   Usage: (hpf-adsr pat attack decay sustain-level release)"
  [pattern attack decay sustain-level release]
  (params pattern {:hpf-env-type :adsr
                   :hpf-attack attack
                   :hpf-decay decay
                   :hpf-s-level sustain-level
                   :hpf-release release}))

(defn hpf-perc
  "Sets HPF percussive envelope parameters at once.
   Usage: (hpf-perc pat attack-sec)"
  [pattern attack-sec]
  (params pattern {:hpf-env-type :perc
                   :hpf-attack attack-sec}))

(defn bpf-adsr
  "Sets all BPF ADSR envelope parameters at once.
   Usage: (bpf-adsr pat attack decay sustain-level release)"
  [pattern attack decay sustain-level release]
  (params pattern {:bpf-env-type :adsr
                   :bpf-attack attack
                   :bpf-decay decay
                   :bpf-s-level sustain-level
                   :bpf-release release}))

(defn bpf-perc
  "Sets BPF percussive envelope parameters at once.
   Usage: (bpf-perc pat attack-sec)"
  [pattern attack-sec]
  (params pattern {:bpf-env-type :perc
                   :bpf-attack attack-sec}))

(defn lpf-env
  "Sets the low-pass filter envelope modulation depth in Hz.
   Usage: (lpf-env pat depth-hz)"
  [pattern depth-hz]
  (set-param pattern :lpf-env depth-hz))

(defn hpf-env
  "Sets the high-pass filter envelope modulation depth in Hz.
   Usage: (hpf-env pat depth-hz)"
  [pattern depth-hz]
  (set-param pattern :hpf-env depth-hz))

(defn bpf-env
  "Sets the band-pass filter envelope modulation depth in Hz.
   Usage: (bpf-env pat depth-hz)"
  [pattern depth-hz]
  (set-param pattern :bpf-env depth-hz))

(defn res-env
  "Sets the filter resonance envelope modulation depth.
   Usage: (res-env pat depth)"
  [pattern depth]
  (set-param pattern :res-env depth))

(defn res-adsr
  "Sets all resonance ADSR envelope parameters at once.
   Usage: (res-adsr pat attack decay sustain-level release)"
  [pattern attack decay sustain-level release]
  (params pattern {:res-env-type :adsr
                   :res-attack attack
                   :res-decay decay
                   :res-s-level sustain-level
                   :res-release release}))

(defn res-perc
  "Sets resonance percussive envelope parameters at once.
   Usage: (res-perc pat attack-sec)"
  [pattern attack-sec]
  (params pattern {:res-env-type :perc
                   :res-attack attack-sec}))

(defn phaser-env
  "Sets the phaser depth envelope modulation amount.
   Usage: (phaser-env pat depth)"
  [pattern depth]
  (set-param pattern :phaser-env depth))

(defn phaser-adsr
  "Sets all phaser ADSR envelope parameters at once.
   Usage: (phaser-adsr pat attack decay sustain-level release)"
  [pattern attack decay sustain-level release]
  (params pattern {:phaser-env-type :adsr
                   :phaser-attack attack
                   :phaser-decay decay
                   :phaser-s-level sustain-level
                   :phaser-release release}))

(defn phaser-perc
  "Sets phaser percussive envelope parameters at once.
   Usage: (phaser-perc pat attack-sec)"
  [pattern attack-sec]
  (params pattern {:phaser-env-type :perc
                   :phaser-attack attack-sec}))

(defn crush-env
  "Sets the bitcrush envelope modulation amount.
   Usage: (crush-env pat depth)"
  [pattern depth]
  (set-param pattern :crush-env depth))

(defn crush-adsr
  "Sets all bitcrush ADSR envelope parameters at once.
   Usage: (crush-adsr pat attack decay sustain-level release)"
  [pattern attack decay sustain-level release]
  (params pattern {:crush-env-type :adsr
                   :crush-attack attack
                   :crush-decay decay
                   :crush-s-level sustain-level
                   :crush-release release}))

(defn crush-perc
  "Sets bitcrush percussive envelope parameters at once.
   Usage: (crush-perc pat attack-sec)"
  [pattern attack-sec]
  (params pattern {:crush-env-type :perc
                   :crush-attack attack-sec}))

(defn fm
  "Sets FM synthesis parameters at once.
   Usage: (fm pat carrier-ratio modulator-ratio mod-index)"
  [pattern carrier-ratio modulator-ratio mod-index]
  (params pattern {:carrier-ratio carrier-ratio
                   :modulator-ratio modulator-ratio
                   :mod-index mod-index}))

(defn echo
  "Sets delay/echo parameters at once.
   Usage: (echo pat delay-time repeats)"
  ([pattern] (echo pattern 0.25 4))
  ([pattern delay-sec] (echo pattern delay-sec 4))
  ([pattern delay-sec repeats]
   (params pattern {:delay delay-sec :repeats repeats})))

;; --- Time/Structural Modifiers ---

(defn fast [pattern amount]
  (let [amount-fn (wrap-number-fn amount)]
    (update pattern :cycles
            (fn [old-val]
              (let [old-fn (wrap-number-fn old-val)]
                (fn [t seed]
                  (* (old-fn t seed) (amount-fn t seed))))))))

(defn slow [pattern amount]
  (let [amount-fn (wrap-number-fn amount)]
    (update pattern :cycles
            (fn [old-val]
              (let [old-fn (wrap-number-fn old-val)]
                (fn [t seed]
                  (/ (old-fn t seed) (amount-fn t seed))))))))

(defn early
  "Shifts the start time of all events in the pattern earlier by amount cycles."
  [pattern cycles]
  (update pattern :events (fn [evs] (map (fn [e] (update e :time - cycles)) evs))))

(defn late
  "Shifts the start time of all events in the pattern later by amount cycles."
  [pattern cycles]
  (update pattern :events (fn [evs] (map (fn [e] (update e :time + cycles)) evs))))

(defn ribbon
  "Loops a specific segment of a pattern.
   offset: start point in cycles
   len: length of segment in cycles"
  [pattern offset-cycles len-cycles]
  (let [source-events (:events pattern)
        ;; Determine the number of cycles in the source pattern
        max-source-t (if (seq source-events)
                       (apply max (map :time source-events))
                       0)
        source-num-cycles (inc (long max-source-t))
        ;; How many cycles do we need to reach 'offset + len'?
        required-cycles (long (Math/ceil (+ offset-cycles len-cycles)))
        ;; Unroll the source events if needed to cover the range
        unroll-count (long (Math/ceil (/ required-cycles (double source-num-cycles))))
        unrolled-events (if (> unroll-count 1)
                          (mapcat (fn [i]
                                    (map (fn [ev]
                                           (update ev :time + (* i source-num-cycles)))
                                         source-events))
                                  (range unroll-count))
                          source-events)
        end (+ offset-cycles len-cycles)
        segment-events (filter (fn [ev]
                                 (let [t (:time ev)]
                                   (and (>= t offset-cycles) (< t end))))
                               unrolled-events)]
    (assoc pattern
           :length len-cycles
           :events
           (map (fn [ev]
                  (let [orig-t (:time ev)
                        new-t (- orig-t offset-cycles)]
                    (-> ev
                        (assoc :time new-t)
                        (update :params
                                (fn [ps]
                                  (reduce-kv (fn [m k v]
                                               (assoc m k
                                                      (if (fn? v)
                                                 ;; Freeze the time for existing functions
                                                 ;; using a stable t derived from orig-t.
                                                 ;; We use * 4.0 to match the beat scale.
                                                        (fn [_t k] (v (* orig-t 4.0) k))
                                                        v)))
                                             {} ps))))))
                segment-events))))

(defn rev
  "Reverses the events within a cycle (0 to 1 range)."
  [pattern]
  (update pattern :events
          (fn [evs]
            (map (fn [e]
                   (let [t (:time e)
                         d (:duration e)
                         cycle-idx (long t)
                         rel-t (- t cycle-idx)
                         new-rel-t (- 1.0 rel-t d)]
                     (assoc e :time (+ cycle-idx new-rel-t))))
                 evs))))

(defn sometimes
  "Randomly applies function f to the pattern with 50% probability per cycle."
  [f pattern]
  (update pattern :events
          (fn [evs]
            (map (fn [e]
                   (let [t (:time e)
                         cycle-idx (long t)
                         r (repeatable-rand cycle-idx :sometimes)]
                     (if (< r 0.5)
                       ((f (make-pattern [e])) :events 0) ; Apply f to single event
                       e)))
                 evs))))

(defn degrade
  "Randomly removes events from the pattern with probability p."
  ([pattern] (degrade pattern 0.5))
  ([pattern probability]
   (update pattern :events
           (fn [evs]
             (filter (fn [e]
                       (let [t (:time e)
                             r (repeatable-rand t :degrade)]
                         (> r probability)))
                     evs)))))

(defn delay-cycles
  "Delays the start of a pattern by n cycles.
   Only affects the initial scheduling (when the pattern is first played)."
  [pattern cycles]
  (assoc pattern :delay-cycles cycles))

(defn stop-after
  "Automatically stops the pattern after n cycles."
  [pattern cycles]
  (assoc pattern :stop-cycles cycles))
