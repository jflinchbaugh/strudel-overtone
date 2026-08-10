(ns strudel-overtone.pattern.signals)

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
