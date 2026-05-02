(ns strudel-overtone.synths
  (:require [overtone.core :as ov]
            [strudel-overtone.pattern :as p]
            [overtone.sc.ugen-collide :as coll])
  (:refer-clojure :exclude [+ - * / < > <= >= min max abs])
  (:refer overtone.sc.ugen-collide :only [+ - * / < > <= >= min max abs]))

(defmacro s-max [min-val val]
  `(ov/clip ~val ~min-val 20000))

(defmacro defsynth [name args & body]
  `(ov/defsynth ~name ~args
     (ov/line:kr 0 0 60 ov/FREE)
     ~@body))

(defonce duck-bus (ov/control-bus))

(defmacro with-glide [freq & body]
  `(let [~'base-f (ov/select:kr ~'monophonic
                               [(ov/line:kr ~'slide-from ~freq ~'slide)
                                (ov/varlag ~freq ~'slide)])
         ~'detuned-f (* ~'base-f (ov/pow 2 (/ ~'detune 1200)))
         ~'actual-f (ov/vibrato:kr ~'detuned-f ~'vibrato 0.02)]
     ~@body))

(defmacro def-strudel-synth [name extra-args & body]
  (let [common-args '{amp 1 sustain 0.2 lpf 2000 resonance 0.1 pan 0
                      detune 0 vibrato 0
                      crush 0 distort 0
                      pshift 0 fshift 0
                      tremolo-hz 0 tremolo-depth 0
                      pan-hz 0 pan-depth 0
                      phaser-hz 0 phaser-depth 0
                      hpf 0 bpf -1 room 0 delay 0 repeats 4
                      duck 0 duck-trigger 0 duck-attack 0.001 duck-release 0.2
                      room-size 0.5 damp 0.5
                      slide 0 slide-from -1
                      legato 1
                      monophonic 0
                      gate 1}
        adsr-defaults '{attack 0.01 decay 0.1 s-level 0.5 release 0.3}
        perc-defaults '{attack 0.01}
        ;; Helper to building smoothed parameters for mono mode
        varlag-param (fn [name]
                       `(ov/select:kr ~'monophonic [~name (ov/varlag ~name ~'slide)]))
        ;; Helper to build the synth definition
        make-synth (fn [suffix env-gen-form env-defaults]
                     (let [extra-map (apply hash-map extra-args)
                           final-args-map (merge
                                           common-args
                                           env-defaults
                                           extra-map)
                           final-args-vec (reduce-kv
                                           (fn [acc k v]
                                             (conj acc k v))
                                           [] final-args-map)]
                       `(do
                          (ov/defsynth ~(symbol (str (clojure.core/name name) suffix))
                            ~final-args-vec
                            (let [;; Smoothed parameters for mono mode
                                  ~'lpf ~(varlag-param 'lpf)
                                  ~'resonance ~(varlag-param 'resonance)
                                  ~'amp ~(varlag-param 'amp)
                                  ~'pan ~(varlag-param 'pan)
                                  ~'hpf ~(varlag-param 'hpf)
                                  ~'bpf ~(varlag-param 'bpf)
                                  ~'crush ~(varlag-param 'crush)
                                  ~'distort ~(varlag-param 'distort)
                                  ~'fshift ~(varlag-param 'fshift)
                                  ~'pshift ~(varlag-param 'pshift)
                                  ~'detune ~(varlag-param 'detune)
                                  ~'vibrato ~(varlag-param 'vibrato)

                                  ~'env ~env-gen-form
                                  ;; Trigger Sidechain
                                  _# (let [trig-env# (ov/env-gen
                                                      (ov/perc
                                                       ~'duck-attack
                                                       ~'duck-release)
                                                      :level-scale
                                                      ~'duck-trigger)]
                                       (ov/out:kr duck-bus trig-env#))
                                  ;; Read Sidechain
                                  ~'duck-env (ov/in:kr duck-bus)
                                  ~'amp-duck (ov/clip
                                              (- 1 (* ~'duck ~'duck-env))
                                              0 1)
                                  ~'snd (do ~@body)
                                  ;; Effect Chain
                                    ;; Pitch Shift bypass
                                    ~'ps (let [~'use-ps (> (ov/absdif ~'pshift 0)
                                                           0.01)
                                               ~'ps-sig (ov/pitch-shift
                                                         ~'snd 0.2
                                                         (ov/pow 2
                                                                 (/ ~'pshift 12)))]
                                           (ov/x-fade2
                                            ~'snd
                                            ~'ps-sig
                                            (ov/lin-lin ~'use-ps 0 1 -1 1)))

                                    ;; Freq Shift bypass
                                    ~'fs (let [~'use-fs (>
                                                         (ov/absdif ~'fshift 0)
                                                         0.01)
                                               ~'fs-sig (ov/freq-shift ~'ps ~'fshift)]
                                           (ov/x-fade2
                                            ~'ps
                                            ~'fs-sig
                                            (ov/lin-lin ~'use-fs 0 1 -1 1)))

                                    ~'trem (let [~'dry ~'fs
                                                 ~'wet (* ~'dry
                                                          (ov/lin-lin
                                                           (ov/sin-osc:kr
                                                            ~'tremolo-hz)
                                                           -1 1
                                                           (- 1 ~'tremolo-depth)
                                                           1))]
                                             (ov/x-fade2
                                              ~'dry
                                              ~'wet
                                              (ov/lin-lin
                                               (> ~'tremolo-depth 0)
                                               0 1 -1 1)))

                                    ~'phs (let [~'dry ~'trem
                                                ~'wet (ov/allpass-n
                                                       ~'dry 0.02
                                                       (ov/lin-lin
                                                        (ov/sin-osc:kr ~'phaser-hz)
                                                        -1 1 0.001 0.01)
                                                       0.1)]
                                            (ov/x-fade2
                                             ~'dry
                                             ~'wet
                                             (ov/lin-lin ~'phaser-depth 0 1 -1 1)))

                                    ~'filt (ov/hpf ~'phs (s-max 20 ~'hpf))
                                    ~'filt (let [~'bpf-sig (ov/bpf
                                                            ~'filt
                                                            (s-max 20 ~'bpf)
                                                            1)]
                                             (ov/x-fade2
                                              ~'filt
                                              ~'bpf-sig
                                              (ov/lin-lin
                                               (> ~'bpf 0)
                                               0 1 -1 1)))
                                    ~'filt (ov/rlpf
                                            ~'filt
                                            (s-max 20 ~'lpf)
                                            ~'resonance)

                                    ~'dst (ov/distort
                                           (* ~'filt
                                              (ov/dbamp (* ~'distort 24))))

                                    ;; Decimator bypass - critical for ringing
                                    ~'crs (let [~'dry ~'dst
                                                ~'wet (ov/decimator
                                                       ~'dry
                                                       (ov/lin-lin
                                                        ~'crush 0 1 44100 2000)
                                                       (ov/lin-lin
                                                        ~'crush 0 1 24 4))]
                                          (ov/x-fade2
                                           ~'dry
                                           ~'wet
                                           (ov/lin-lin
                                            (> ~'crush 0)
                                            0 1 -1 1)))

                                    ~'gated (* ~'crs ~'env)
                                    ~'dly (let [~'dry ~'gated
                                                ~'wet (+ ~'dry
                                                         (ov/comb-n
                                                          ~'dry 2.0
                                                          (s-max 0.0001 ~'delay)
                                                          (*
                                                           ~'delay
                                                           ~'repeats)))]
                                            (ov/x-fade2
                                             ~'dry
                                             ~'wet
                                             (ov/lin-lin
                                              (> ~'delay 0)
                                              0 1 -1 1)))

                                    ~'reverbed (ov/free-verb
                                                ~'dly
                                                ~'room
                                                ~'room-size
                                                ~'damp)
                                    _# (ov/detect-silence
                                        ~'reverbed
                                        :amp 0.0001
                                        :time 0.2
                                        :action ov/FREE)
                                    ~'actual-pan (+
                                                  ~'pan
                                                  (let [~'mod (*
                                                               (ov/sin-osc:kr ~'pan-hz)
                                                               ~'pan-depth)]
                                                    (*
                                                     ~'mod
                                                     (> ~'pan-depth 0))))]
                                (ov/out 0
                                        (ov/pan2
                                         (*
                                          (ov/mix [~'reverbed])
                                          ~'amp
                                          ~'amp-duck)
                                         ~'actual-pan))))
                          (let [synth-var# (var ~(symbol (str (clojure.core/name name) suffix)))]
                            (when-not (= *ns* (find-ns 'strudel-overtone.synths))
                              (intern 'strudel-overtone.synths
                                      (quote ~(symbol (str (clojure.core/name name) suffix)))
                                      synth-var#))))))]

    `(do
       ~(make-synth "-adsr"
                    `(let [auto-gate# (ov/line:kr 1 0 ~'sustain)
                           effective-gate# (ov/select:kr ~'monophonic [auto-gate# ~'gate])]
                       (ov/env-gen (ov/adsr ~'attack ~'decay ~'s-level ~'release)
                                   :gate effective-gate#))
                    adsr-defaults)
       ~(make-synth "-perc"
                    `(let [auto-gate# (ov/line:kr 1 0 ~'sustain)
                           effective-gate# (ov/select:kr ~'monophonic [auto-gate# ~'gate])]
                       (ov/env-gen (ov/perc ~'attack ~'sustain)
                                   :gate effective-gate#))
                    perc-defaults))))

(defmacro def-additive!
  "Defines a new additive synth from a sequence of harmonic volume ratios.
   The resulting synth will be named `name` (e.g. :my-synth) and
   will have the usual strudel-overtone parameters available.
   Optionally accepts a :step size between harmonics (defaults to 1)."
  [name ratios & {:keys [step] :or {step 1}}]
  (let [ratios-val (if (vector? ratios) ratios (eval ratios))]
    `(def-strudel-synth ~name [~'freq 440 ~'step ~step]
       (with-glide ~'freq
         (ov/mix
          (map-indexed
           (fn [i# r#]
             (* r# (ov/sin-osc (* ~'actual-f (+ 1 (* i# ~'step))))))
           ~ratios-val))))))

(def-strudel-synth kick [freq 50]
  (let [f-env (ov/line:kr (* 8 freq) freq 0.02)
        click (* 0.5 (ov/env-gen (ov/perc 0.001 0.01)) (ov/white-noise))
        body (ov/sin-osc f-env)]
    (ov/tanh (* 2.5 (+ body click)))))

(def-strudel-synth snare [freq 200]
  (let [noise (ov/white-noise)
        body (ov/sin-osc (ov/line:kr (* 2 freq) freq 0.05))]
    (+ (* 0.4 body (ov/env-gen (ov/perc 0.001 0.1)))
       (* 0.8 noise))))

(def-strudel-synth hat [freq 8000]
  (ov/white-noise))

(def-strudel-synth clap [freq 1200]
  (ov/bpf (ov/white-noise) freq resonance))

(def-strudel-synth saw [freq 440]
  (with-glide freq
    (ov/saw actual-f)))

(def-strudel-synth sine [freq 440]
  (with-glide freq
    (ov/sin-osc actual-f)))

(def-strudel-synth square [freq 440 width 0.5]
  (with-glide freq
    (ov/pulse actual-f width)))

(def-strudel-synth tri [freq 440]
  (with-glide freq
    (ov/lf-tri actual-f)))

(def-strudel-synth fm
  [freq 440
   carrier-ratio 1
   modulator-ratio 2
   mod-index 5]
  (with-glide freq
    (let [modulator (ov/sin-osc (* actual-f modulator-ratio))
          carrier (ov/sin-osc (+ (* actual-f carrier-ratio)
                                 (* modulator mod-index actual-f)))]
      carrier)))

(def-strudel-synth white [freq 440] (ov/white-noise))
(def-strudel-synth pink [freq 440] (ov/pink-noise))
(def-strudel-synth brown [freq 440] (ov/brown-noise))
(def-strudel-synth gray [freq 440] (ov/gray-noise))
(def-strudel-synth clip [freq 440] (ov/clip-noise))

(def-strudel-synth crackle
  [freq 440
   chaos 1.5]
  (ov/crackle chaos))

(def-strudel-synth dust [freq 440]
  (with-glide freq
    (ov/dust actual-f)))

(def-strudel-synth dust2 [freq 440]
  (with-glide freq
    (ov/dust2 actual-f)))

(def-strudel-synth lf-noise0 [freq 440]
  (with-glide freq
    (ov/lf-noise0 actual-f)))

(def-strudel-synth lf-noise1 [freq 440]
  (with-glide freq
    (ov/lf-noise1 actual-f)))

(def-strudel-synth lf-noise2 [freq 440]
  (with-glide freq
    (ov/lf-noise2 actual-f)))

(def-strudel-synth tb303 [freq 440 wave 1 env-amount 1000]
  (with-glide freq
    (let [freqs [actual-f (* 1.01 actual-f)]
          waves [(ov/saw freqs)
                 (ov/pulse freqs 0.5)
                 (ov/lf-tri freqs)]
          selector (ov/select wave waves)
          fil-env (ov/env-gen (ov/perc attack sustain))
          fil-lpf (s-max 20 (+ lpf (* env-amount fil-env)))]
      (ov/rlpf selector fil-lpf (ov/lin-lin resonance 0 1 0.9 0.05)))))

(def-strudel-synth supersaw [freq 440]
  (with-glide freq
    (let [detunes [1.0 1.001 0.999 1.005 0.995 1.01 0.99]
          saws (map #(ov/saw (* actual-f %)) detunes)]
      (ov/leak-dc:ar (* 0.3 (ov/mix saws))))))

(def-strudel-synth mooger
  [freq 440
   osc1 0
   osc2 1
   osc1-level 0.5
   osc2-level 0.5]
  (with-glide freq
    (let [osc-bank-1 [(ov/saw actual-f) (ov/sin-osc actual-f) (ov/pulse actual-f)]
          osc-bank-2 [(ov/saw actual-f) (ov/sin-osc actual-f) (ov/pulse actual-f)]
          s1 (* osc1-level (ov/select osc1 osc-bank-1))
          s2 (* osc2-level (ov/select osc2 osc-bank-2))]
      (ov/moog-ff (+ s1 s2) lpf (ov/lin-lin resonance 0 1 3 0)))))

(def-strudel-synth ks-stringer [freq 440 coef 0.5]
  (with-glide freq
    (let [noise (* 0.8 (ov/white-noise))
          ;; Excitation burst
          burst (* noise (ov/env-gen (ov/perc 0.001 0.01)))
          ;; Waveguide string modeling
          delay-time (/ 1.0 actual-f)
          ;; Use comb-l for a delay line that supports real-time modulation of delay-time
          string (ov/comb-l burst 0.1 delay-time (ov/lin-lin coef -1 1 0.1 10))]
      string)))

(def-strudel-synth dub-kick [freq 45]
  (let [f-env (ov/line:kr (* 4 freq) freq 0.05)
        body (ov/sin-osc f-env)
        noiz (* 0.2 (ov/env-gen (ov/perc 0.01 0.2)) (ov/white-noise))]
    (ov/tanh (* 2.0 (+ body noiz)))))

(def-strudel-synth dance-kick [freq 60]
  (let [f-env (ov/line:kr (* 12 freq) freq 0.03)
        click (* 0.8 (ov/env-gen (ov/perc 0.001 0.02)) (ov/pink-noise))
        body (ov/sin-osc f-env)]
    (ov/tanh (* 5.0 (+ body click)))))

(def-strudel-synth sampler [buf 0 rate 1 begin 0 end 1 loop? 0 attack 0 release 0]
  (let [rate-s (* rate (ov/buf-rate-scale buf))
        start-pos (* begin (ov/buf-frames buf))]
    (ov/play-buf 2 buf rate-s 1 start-pos loop? :action ov/NO-ACTION)))

(def synth-aliases
  {:bd :kick
   :sd :snare
   :hh :hat
   :cp :clap})

(def percussive-synths
  #{:kick :snare :hat :clap :bd :sd :hh :cp :dub-kick :dance-kick :ks-stringer})

(defn supports-mono? [sound-name]
  (not (or (percussive-synths sound-name)
           (#{:sampler :white :pink :brown} sound-name))))

(defn get-synth-name [sound params]
  (let [base (get synth-aliases sound sound)
        default-env (if (percussive-synths base) :perc :adsr)
        env (get params :env default-env)]
    (keyword (str (clojure.core/name base) "-" (clojure.core/name env)))))

(defn resolve-synth [name]
  (let [s (symbol (clojure.core/name name))]
    (when-let [ns (find-ns 'strudel-overtone.synths)]
      (ns-resolve ns s))))
