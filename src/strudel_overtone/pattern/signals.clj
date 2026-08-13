(ns strudel-overtone.pattern.signals)

(defn- tau [t] (* 2 Math/PI t))

(defn sig-range
  "Wraps a signal function to scale its output to [low, high]."
  [sig low high]
  (fn [t seed]
    (let [v (sig t seed)]
      (+ low (* v (- high low))))))

(defmacro def-sig
  "Defines a signal function with standard 4 arities:
   [] -> signal at frequency 1
   [t seed] -> value at time t
   [freq] -> signal function at given frequency
   [freq low high] -> signal function scaled to range [low, high]"
  [name doc-string [t-sym seed-sym] & body]
  `(defn ~name
     ~doc-string
     ([] (~name 1))
     ([~t-sym ~seed-sym]
      ~@body)
     ([freq#]
      (fn [t# seed#]
        (~name (* t# freq#) seed#)))
     ([freq# low# high#]
      (sig-range (~name freq#) low# high#))))

(def-sig sine-sig
  "Returns a continuous sine wave signal (0 to 1)."
  [t _]
  (+ 0.5 (* 0.5 (Math/sin (tau t)))))

(def-sig saw-sig
  "Returns a continuous sawtooth wave signal (0 to 1)."
  [t _]
  (mod t 1))

(def-sig tri-sig
  "Returns a continuous triangle wave signal (0 to 1)."
  [t _]
  (let [x (mod t 1)]
    (if (< x 0.5)
      (* 2 x)
      (- 2 (* 2 x)))))

(def-sig square-sig
  "Returns a continuous square wave signal (0 to 1)."
  [t _]
  (if (< (mod t 1) 0.5) 1 0))

(def-sig cosine-sig
  "Returns a continuous cosine wave signal (0 to 1)."
  [t _]
  (+ 0.5 (* 0.5 (Math/cos (tau t)))))

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
