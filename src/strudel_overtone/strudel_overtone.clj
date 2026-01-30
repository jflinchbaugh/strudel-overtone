(ns strudel-overtone.strudel-overtone
  (:require [overtone.core :as ov :refer :all :exclude [note lpf decay]]
            [taoensso.telemere :as tel]
            [clojure.string :as str]))

;; --- Synths ---

(defsynth kick [amp 1 sustain 0.3 freq 60 cutoff 3000]
  (let [env (env-gen (perc 0.01 sustain) :action FREE)
        snd (ov/lpf (sin-osc (line:kr (* 2 freq) freq 0.1)) cutoff)]
    (out 0 (pan2 (* snd env amp) 0))))

(defsynth snare [amp 1 sustain 0.2 freq 200 cutoff 3000]
  (let [env (env-gen (perc 0.01 sustain) :action FREE)
        noise (ov/lpf (white-noise) cutoff)
        snd (+ (* 0.5) (* 0.8 noise))]
    (out 0 (pan2 (* snd env amp) 0))))

(defsynth hat [amp 1 sustain 0.1 freq 8000 cutoff 6000]
  (let [env (env-gen (perc 0.001 sustain) :action FREE)
        snd (hpf (white-noise) cutoff)]
    (out 0 (pan2 (* snd env amp) 0))))

(defsynth clap [amp 1 sustain 0.1 freq 1200 cutoff 1500 resonance 0.2]
  (let [env (env-gen (perc 0.005 sustain) :action FREE)
        snd (bpf (white-noise) freq resonance)]
    (out 0 (pan2 (* snd env amp) 0))))

(defsynth saw-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                    attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (saw f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth sine-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                     attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (sin-osc f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth square-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                       attack 0.01 decay 0.1 s-level 0.5 release 0.3 width 0.5 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (pulse f width)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth tri-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                    attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (lf-tri f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth fm-adsr [freq 440
                   amp 1
                   sustain 0.5
                   carrier-ratio 1
                   modulator-ratio 2
                   mod-index 5
                   cutoff 2000
                   resonance 0.1
                   pan 0
                   attack 0.01
                   decay 0.1
                   s-level 0.7
                   release 0.3
                   detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        modulator (sin-osc (* f modulator-ratio))
        carrier (sin-osc (+ (* f carrier-ratio) (* modulator mod-index (* f))))
        filt (rlpf carrier cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth fm-perc [freq 440
                   amp 1
                   sustain 0.2
                   carrier-ratio 1
                   modulator-ratio 2
                   mod-index 5
                   cutoff 2000
                   resonance 0.1
                   pan 0
                   attack 0.01
                   detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        modulator (sin-osc (* f modulator-ratio))
        carrier (sin-osc (+ (* f carrier-ratio) (* modulator mod-index (* f))))
        filt (rlpf carrier cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth saw-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (saw f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth sine-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (sin-osc f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth square-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 width 0.5 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (pulse f width)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth tri-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (lf-tri f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

;; --- Noise Synths ---

(defsynth white-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                      attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        snd (white-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth white-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        snd (white-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth pink-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                     attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        snd (pink-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth pink-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        snd (pink-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth brown-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                      attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        snd (brown-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth brown-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        snd (brown-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth gray-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                     attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        snd (gray-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth gray-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        snd (gray-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth clip-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                     attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        snd (clip-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth clip-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        snd (clip-noise)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth crackle-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                        attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0 chaos 1.5]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        snd (crackle chaos)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth crackle-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0 chaos 1.5]
  (let [env (env-gen (perc attack sustain) :action FREE)
        snd (crackle chaos)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth dust-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                     attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (dust f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth dust-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (dust f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth dust2-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                      attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (dust2 f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth dust2-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (dust2 f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth lf-noise0-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                          attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (lf-noise0 f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth lf-noise0-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (lf-noise0 f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth lf-noise1-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                          attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (lf-noise1 f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth lf-noise1-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (lf-noise1 f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth lf-noise2-adsr [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                          attack 0.01 decay 0.1 s-level 0.5 release 0.3 detune 0]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (lf-noise2 f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

(defsynth lf-noise2-perc [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0 attack 0.01 detune 0]
  (let [env (env-gen (perc attack sustain) :action FREE)
        f (* freq (ov/midiratio (/ detune 100)))
        snd (lf-noise2 f)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

;; --- Pattern Engine ---

(defrecord Event [time duration params])
(defrecord Pattern [events cycles])

(defn make-pattern [events]
  (->Pattern events 1))

(defn parse-mini
  "Naively parses a collection into a sequence of events with duration.
   Returns a list of maps {:value v :start s :duration d}.
   Handles nested collections by subdividing the duration."
  ([tokens] (parse-mini tokens 0.0 1.0))
  ([tokens start duration]
   (let [n (count tokens)
         dur (if (pos? n) (/ duration (double n)) 0)]
     (mapcat (fn [[i v]]
               (let [s-time (+ start (* i dur))]
                 (if (and (sequential? v) (not (string? v)))
                   (parse-mini v s-time dur)
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
           (mapv (fn [be]
                   (let [mid (+ (:time be) (/ (:duration be) 2))
                         match (some (fn [ne]
                                       (let [s (:time ne)
                                             e (+ s (:duration ne))]
                                         (when (and (<= s mid) (< mid e))
                                           ne)))
                                     new-events)]
                     (if match
                       (assoc-in be [:params key] (get-in match [:params key]))
                       be)))
                 base-events))))

(defn set-param
  ([pattern key val] (set-param pattern key val try-parse-number))
  ([pattern key val transform-fn]
   (if (sequential? val)
     (combine-patterns pattern (make-pattern (make-event-list val key transform-fn)) key)
     (with-param pattern key (transform-fn val)))))

(defn s
  "Creates a pattern from a sound string (mini-notation),
  or sets the sound of an existing pattern."
  ([pat]
   (make-pattern (make-event-list pat :sound ->name)))
  ([pattern sound-val]
   (set-param pattern :sound sound-val ->name)))

(defn note
  "Creates a pattern from a note string (mini-notation), or sets the note of an existing pattern."
  ([pat]
   (make-pattern (make-event-list pat :note identity)))
  ([pattern note-val]
   (set-param pattern :note note-val identity)))

(defn gain [pattern val] (set-param pattern :amp val))
(defn lpf [pattern val] (set-param pattern :cutoff val))
(defn pan [pattern val] (set-param pattern :pan val))
(defn resonance [pattern val] (set-param pattern :resonance val))
(defn attack [pattern val] (set-param pattern :attack val))
(defn decay [pattern val] (set-param pattern :decay val))
(defn s-level [pattern val] (set-param pattern :s-level val))
(defn release [pattern val] (set-param pattern :release val))
(defn width [pattern val] (set-param pattern :width val))
(defn carrier-ratio [pattern val] (set-param pattern :carrier-ratio val))
(defn modulator-ratio [pattern val] (set-param pattern :modulator-ratio val))
(defn mod-index [pattern val] (set-param pattern :mod-index val))
(defn detune [pattern val] (set-param pattern :detune val))
(defn add [pattern val] (set-param pattern :add val))
(defn chaos [pattern val] (set-param pattern :chaos val))
(defn env
  "Sets the envelope of a pattern. Can be a single value or a sequence/mini-notation."
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
            cutoff (let [c (or (:cutoff params) 2000)]
                     (if (string? c)
                       (try (Double/parseDouble c)
                            (catch Exception _ 2000))
                       c))
            ;; Calculate sustain in seconds from beats
            sustain-sec (* dur-beats (/ 60 (metro-bpm metro)))
            ;; Default sound if only note is provided
            sound-name (or sound-param (if n "saw" nil))]

        (when sound-name
          (let [base (get synth-aliases sound-name sound-name)
                synth-key (get-synth-name base params)
                synth-var (or
                           (resolve-synth synth-key)
                           (resolve-synth base))
                freq (if n (resolve-note (+ (if (keyword? n) (ov/note n) n) note-offset)) nil)
                reserved #{:sound :note :active :start :duration :env :add}
                handled #{:amp :cutoff :sustain :freq}
                args (cond-> (reduce-kv (fn [acc k v]
                                          (if (or (reserved k) (handled k))
                                            acc
                                            (conj acc k v)))
                                        []
                                        params)
                       true (conj :amp amp)
                       freq (conj :freq freq)
                       cutoff (conj :cutoff cutoff)
                       sustain-sec (conj :sustain sustain-sec))]
            (when synth-var
              (do
                (apply-at (metro beat)
                          (fn [& e] (tel/log! :info {:event (into {} e)})) ev)
                (apply-at (metro beat) synth-var args)))))))))

(defn play-loop [key beat]
  (let [state @player-state]
    (if (and (:playing? state)
             (contains? (:loops state) key))
      (let [pat (get-in state [:patterns key])]
        (if pat
          (let [cycles (:cycles pat 1) ;; Speed multiplier
                cycle-dur (/ 4 cycles) ;; Beats per cycle (assuming 4/4)
                next-beat (+ beat cycle-dur)]

            ;; Schedule events for this cycle
            (doseq [ev (:events pat)]
              (let [rel-start (:time ev)
                    rel-dur (:duration ev)
                    ev-beat (+ beat (* rel-start cycle-dur))
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
          (lpf (map (partial * 1000) (concat (range 0.0 1.0 0.05) (range 1.0 0.0 -0.05))))
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
          (gain [0.2] #_(take 4 (chosen-from (map (partial * 1/16) (range 16))))))

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
        (pan 0.8)
        (active 0 #_(chosen-from [0 1 1] 8)))
   :bd-4 (->
          (s [:bd :bd :bd :bd])
          (pan -0.8)
          (gain 1)
          (active 1))
   :sd (->
        (s (take 16 (cycle [:- :sd])))
        (gain 0.2)
        (active 0))
   :clap (->
          (s (repeat 16 :hh))
          (env :perc)
          (gain 0.2)
          (active 0 #_(chosen-from [0 1 0 1] 16)))
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
          (note (chosen-from (take 15 (scale :d4 :minor)) 8))
          (chaos 1.5)
          (env (chosen-from [:perc] 4))
          (s (chosen-from [:crackle] 2))
          (lpf (chosen-from [50 100 200 400 800] 8))
          (gain (chosen-from (range 0.5 1.6 0.05) 16))
          (pan (chosen-from (range -0.9 0.9 0.2) 16))
          (active (chosen-from [1] 8))
          (fast 1)))

  (stop!)

  (connect-server)

  (recording-start "song.wav")

  (recording-stop)
  (ov/midiratio (/ 100 100))

  .)

