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
  `(let [~'start-f (ov/select:kr (< ~'slide-from 0) [~'slide-from ~freq])
         ~'base-f (ov/select:kr ~'monophonic
                               [(ov/line:kr ~'slide-from ~freq ~'slide)
                                (ov/varlag ~freq ~'slide 0 1 ~'start-f)])
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
                      gate 1
                      env-type 0
                      attack 0.01 decay 0.1 s-level 0.5 release 0.3
                      lpf-env 0
                      lpf-env-type 0
                      lpf-attack 0.01 lpf-decay 0.1 lpf-s-level 0.5 lpf-release 0.3
                      hpf-env 0
                      hpf-env-type 0
                      hpf-attack 0.01 hpf-decay 0.1 hpf-s-level 0.5 hpf-release 0.3
                      bpf-env 0
                      bpf-env-type 0
                      bpf-attack 0.01 bpf-decay 0.1 bpf-s-level 0.5 bpf-release 0.3
                      res-env 0
                      res-env-type 0
                      res-attack 0.01 res-decay 0.1 res-s-level 0.5 res-release 0.3
                      phaser-env 0
                      phaser-env-type 0
                      phaser-attack 0.01 phaser-decay 0.1 phaser-s-level 0.5 phaser-release 0.3
                      crush-env 0
                      crush-env-type 0
                      crush-attack 0.01 crush-decay 0.1 crush-s-level 0.5 crush-release 0.3
                      detune-env 0
                      detune-env-type 0
                      detune-attack 0.01 detune-decay 0.1 detune-s-level 0.5 detune-release 0.3
                      pshift-env 0
                      pshift-env-type 0
                      pshift-attack 0.01 pshift-decay 0.1 pshift-s-level 0.5 pshift-release 0.3
                      fshift-env 0
                      fshift-env-type 0
                      fshift-attack 0.01 fshift-decay 0.1 fshift-s-level 0.5 fshift-release 0.3
                      pan-env 0
                      pan-env-type 0
                      pan-attack 0.01 pan-decay 0.1 pan-s-level 0.5 pan-release 0.3
                      distort-env 0
                      distort-env-type 0
                      distort-attack 0.01 distort-decay 0.1 distort-s-level 0.5 distort-release 0.3}
        varlag-param (fn [name]
                       `(ov/select:kr ~'monophonic [~name (ov/varlag ~name ~'slide)]))
        extra-map (apply hash-map extra-args)
        final-args-map (merge common-args extra-map)
        final-args-vec (reduce-kv (fn [acc k v] (conj acc k v)) [] final-args-map)
        synth-symbol (symbol (clojure.core/name name))
        doc-str (str "Strudel synth voice: " (clojure.core/name name))]
    `(do
       (ov/defsynth ~synth-symbol
         ~final-args-vec
         (let [;; Smoothed parameters for mono mode
               ~'lpf ~(varlag-param 'lpf)
               ~'lpf-env ~(varlag-param 'lpf-env)
               ~'hpf ~(varlag-param 'hpf)
               ~'hpf-env ~(varlag-param 'hpf-env)
               ~'bpf ~(varlag-param 'bpf)
               ~'bpf-env ~(varlag-param 'bpf-env)
               ~'resonance ~(varlag-param 'resonance)
               ~'res-env ~(varlag-param 'res-env)
               ~'phaser-env ~(varlag-param 'phaser-env)
               ~'crush ~(varlag-param 'crush)
               ~'crush-env ~(varlag-param 'crush-env)
               ~'detune ~(varlag-param 'detune)
               ~'detune-env ~(varlag-param 'detune-env)
               ~'pshift ~(varlag-param 'pshift)
               ~'pshift-env ~(varlag-param 'pshift-env)
               ~'fshift ~(varlag-param 'fshift)
               ~'fshift-env ~(varlag-param 'fshift-env)
               ~'pan ~(varlag-param 'pan)
               ~'pan-env ~(varlag-param 'pan-env)
               ~'distort ~(varlag-param 'distort)
               ~'distort-env ~(varlag-param 'distort-env)
               ~'amp ~(varlag-param 'amp)
               ~'vibrato ~(varlag-param 'vibrato)

               auto-gate# (ov/line:kr 1 0 ~'sustain)
               effective-gate# (ov/select:kr ~'monophonic [auto-gate# ~'gate])
               adsr-env# (ov/env-gen (ov/adsr ~'attack ~'decay ~'s-level ~'release) :gate effective-gate#)
               perc-env# (ov/env-gen (ov/perc ~'attack ~'sustain) :gate effective-gate#)
               ~'env (ov/select:kr ~'env-type [adsr-env# perc-env#])

               lpf-adsr# (ov/env-gen (ov/adsr ~'lpf-attack ~'lpf-decay ~'lpf-s-level ~'lpf-release) :gate effective-gate#)
               lpf-perc# (ov/env-gen (ov/perc ~'lpf-attack ~'sustain) :gate effective-gate#)
               lpf-env-sig# (ov/select:kr ~'lpf-env-type [lpf-adsr# lpf-perc#])
               effective-lpf# (s-max 20 (+ ~'lpf (* ~'lpf-env lpf-env-sig#)))

               hpf-adsr# (ov/env-gen (ov/adsr ~'hpf-attack ~'hpf-decay ~'hpf-s-level ~'hpf-release) :gate effective-gate#)
               hpf-perc# (ov/env-gen (ov/perc ~'hpf-attack ~'sustain) :gate effective-gate#)
               hpf-env-sig# (ov/select:kr ~'hpf-env-type [hpf-adsr# hpf-perc#])
               effective-hpf# (s-max 20 (+ ~'hpf (* ~'hpf-env hpf-env-sig#)))

               bpf-adsr# (ov/env-gen (ov/adsr ~'bpf-attack ~'bpf-decay ~'bpf-s-level ~'bpf-release) :gate effective-gate#)
               bpf-perc# (ov/env-gen (ov/perc ~'bpf-attack ~'sustain) :gate effective-gate#)
               bpf-env-sig# (ov/select:kr ~'bpf-env-type [bpf-adsr# bpf-perc#])
               effective-bpf# (s-max 20 (+ ~'bpf (* ~'bpf-env bpf-env-sig#)))

               res-adsr# (ov/env-gen (ov/adsr ~'res-attack ~'res-decay ~'res-s-level ~'res-release) :gate effective-gate#)
               res-perc# (ov/env-gen (ov/perc ~'res-attack ~'sustain) :gate effective-gate#)
               res-env-sig# (ov/select:kr ~'res-env-type [res-adsr# res-perc#])
               effective-res# (ov/clip (+ ~'resonance (* ~'res-env res-env-sig#)) 0.001 1.0)

               phaser-adsr# (ov/env-gen (ov/adsr ~'phaser-attack ~'phaser-decay ~'phaser-s-level ~'phaser-release) :gate effective-gate#)
               phaser-perc# (ov/env-gen (ov/perc ~'phaser-attack ~'sustain) :gate effective-gate#)
               phaser-env-sig# (ov/select:kr ~'phaser-env-type [phaser-adsr# phaser-perc#])
               effective-phaser-depth# (ov/clip (+ ~'phaser-depth (* ~'phaser-env phaser-env-sig#)) 0 1)

               crush-adsr# (ov/env-gen (ov/adsr ~'crush-attack ~'crush-decay ~'crush-s-level ~'crush-release) :gate effective-gate#)
               crush-perc# (ov/env-gen (ov/perc ~'crush-attack ~'sustain) :gate effective-gate#)
               crush-env-sig# (ov/select:kr ~'crush-env-type [crush-adsr# crush-perc#])
               effective-crush# (ov/clip (+ ~'crush (* ~'crush-env crush-env-sig#)) 0 1)

               detune-adsr# (ov/env-gen (ov/adsr ~'detune-attack ~'detune-decay ~'detune-s-level ~'detune-release) :gate effective-gate#)
               detune-perc# (ov/env-gen (ov/perc ~'detune-attack ~'sustain) :gate effective-gate#)
               detune-env-sig# (ov/select:kr ~'detune-env-type [detune-adsr# detune-perc#])
               ~'effective-detune (+ ~'detune (* ~'detune-env detune-env-sig#))

               pshift-adsr# (ov/env-gen (ov/adsr ~'pshift-attack ~'pshift-decay ~'pshift-s-level ~'pshift-release) :gate effective-gate#)
               pshift-perc# (ov/env-gen (ov/perc ~'pshift-attack ~'sustain) :gate effective-gate#)
               pshift-env-sig# (ov/select:kr ~'pshift-env-type [pshift-adsr# pshift-perc#])
               effective-pshift# (+ ~'pshift (* ~'pshift-env pshift-env-sig#))

               fshift-adsr# (ov/env-gen (ov/adsr ~'fshift-attack ~'fshift-decay ~'fshift-s-level ~'fshift-release) :gate effective-gate#)
               fshift-perc# (ov/env-gen (ov/perc ~'fshift-attack ~'sustain) :gate effective-gate#)
               fshift-env-sig# (ov/select:kr ~'fshift-env-type [fshift-adsr# fshift-perc#])
               effective-fshift# (+ ~'fshift (* ~'fshift-env fshift-env-sig#))

               pan-adsr# (ov/env-gen (ov/adsr ~'pan-attack ~'pan-decay ~'pan-s-level ~'pan-release) :gate effective-gate#)
               pan-perc# (ov/env-gen (ov/perc ~'pan-attack ~'sustain) :gate effective-gate#)
               pan-env-sig# (ov/select:kr ~'pan-env-type [pan-adsr# pan-perc#])
               effective-pan# (ov/clip (+ ~'pan (* ~'pan-env pan-env-sig#)) -1 1)

               distort-adsr# (ov/env-gen (ov/adsr ~'distort-attack ~'distort-decay ~'distort-s-level ~'distort-release) :gate effective-gate#)
               distort-perc# (ov/env-gen (ov/perc ~'distort-attack ~'sustain) :gate effective-gate#)
               distort-env-sig# (ov/select:kr ~'distort-env-type [distort-adsr# distort-perc#])
               effective-distort# (ov/clip (+ ~'distort (* ~'distort-env distort-env-sig#)) 0 1)

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
                 ~'ps (let [~'use-ps (> (ov/absdif effective-pshift# 0)
                                        0.01)
                            ~'ps-sig (ov/pitch-shift
                                      ~'snd 0.2
                                      (ov/pow 2
                                              (/ effective-pshift# 12)))]
                        (ov/x-fade2
                         ~'snd
                         ~'ps-sig
                         (ov/lin-lin ~'use-ps 0 1 -1 1)))

                 ;; Freq Shift bypass
                 ~'fs (let [~'use-fs (>
                                      (ov/absdif effective-fshift# 0)
                                      0.01)
                            ~'fs-sig (ov/freq-shift ~'ps effective-fshift#)]
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
                          (ov/lin-lin effective-phaser-depth# 0 1 -1 1)))

                 ~'filt (ov/hpf ~'phs effective-hpf#)
                 ~'filt (let [~'bpf-rq (ov/lin-lin effective-res# 0 1 1.0 0.05)
                              ~'bpf-sig (ov/bpf
                                         ~'filt
                                         effective-bpf#
                                         ~'bpf-rq)]
                          (ov/x-fade2
                           ~'filt
                           ~'bpf-sig
                           (ov/lin-lin
                            (> ~'bpf 0)
                            0 1 -1 1)))
                 ~'filt (ov/rlpf
                         ~'filt
                         effective-lpf#
                         effective-res#)

                 ~'dst (ov/distort
                        (* ~'filt
                           (ov/dbamp (* effective-distort# 24))))

                 ;; Decimator bypass - critical for ringing
                 ~'crs (let [~'dry ~'dst
                             ~'wet (ov/decimator
                                    ~'dry
                                    (ov/lin-lin
                                     effective-crush# 0 1 44100 2000)
                                    (ov/lin-lin
                                     effective-crush# 0 1 24 4))]
                       (ov/x-fade2
                        ~'dry
                        ~'wet
                        (ov/lin-lin
                         (> effective-crush# 0)
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
                      (ov/select:ar ~'monophonic
                                    [~'reverbed 1.0])
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
       (alter-meta! (var ~synth-symbol) assoc :doc ~doc-str)
       (let [synth-var# (var ~synth-symbol)]
         (when-not (= *ns* (find-ns 'strudel-overtone.synths))
           (intern 'strudel-overtone.synths
                   (quote ~synth-symbol)
                   synth-var#))))))

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
  (let [;; A simpler multi-burst envelope for the clap
        env (ov/env-gen (ov/envelope [0 1 0 0.9 0 0.8 0]
                                     [0.005 0.01 0.005 0.01 0.005 0.2]
                                     -4))
        noise (ov/white-noise)]
    (* (ov/hpf noise freq) env)))

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

(defn supports-mono?
  "Checks if a synth sound supports monophonic play mode."
  [sound-name]
  (not (or (percussive-synths sound-name)
           (#{:sampler :white :pink :brown} sound-name))))

(defn get-synth-name
  "Resolves the synth name keyword, mapping aliases (e.g., :bd -> :kick)."
  [sound _params]
  (get synth-aliases sound sound))

(defn resolve-synth
  "Resolves a synth var symbol by name keyword within the synths namespace."
  [name]
  (let [s (symbol (clojure.core/name name))]
    (when-let [ns (find-ns 'strudel-overtone.synths)]
      (ns-resolve ns s))))
