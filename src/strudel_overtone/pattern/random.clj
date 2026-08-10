(ns strudel-overtone.pattern.random)

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
