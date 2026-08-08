(ns strudel-overtone.player
  (:require [overtone.core :as ov]
            [taoensso.telemere :as tel]
            [tick.core :as t]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.synths :as synths]
            [strudel-overtone.samples :as samples]))

;; --- Logging ---

(def ^:private time-formatter
  (t/formatter "HH:mm:ss.SSS"))

(tel/stop-handlers!)

(tel/add-handler!
 :console
 (tel/handler:console
  {:output-fn
   (fn [signal]
     (let [{:keys [inst msg_ data]} signal]
       (str (t/format time-formatter (t/zoned-date-time inst)) " "
            (or (force msg_) data)
            "\n")))}))

(defonce metro (ov/metronome 120))

(defn cpm
  "Sets or gets the cycles per minute.
   Assumes 4 beats per cycle."
  ([] (/ (ov/metro-bpm metro) 4))
  ([n]
   (metro :bpm (* n 4))
   (tel/log! :info {:cpm n})
   n))

(defn glide-cpm
  "Smoothly transitions the CPM to a new value over a duration (in cycles).
   Default resolution is 1 step per cycle."
  ([target-cpm dur-cycles]
   (glide-cpm target-cpm dur-cycles 1))
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
         (ov/apply-at (metro (+ now beat-offset))
                      (fn []
                        (tel/log! :info {:cpm target-val})
                        (cpm target-val))))))))

(defonce player-state (atom {:playing? false :patterns {} :loops #{} :last-freq {}}))

(defn- resolve-note [n]
  (if (number? n)
    (ov/midi->hz n)
    (ov/midi->hz (ov/note n))))

(defn resolve-params [params beat cycle]
  (binding [p/*current-cycle* (or cycle 0)]
    (let [cycle-t (/ (double beat) 4.0)]
      (reduce-kv (fn [m k v]
                   (assoc m k (if (fn? v) (v cycle-t k) v)))
                 {}
                 params))))

(defn- is-active? [v]
  (if (number? v) (not (zero? v)) v))

(defn- try-parse-number [v]
  (if (string? v)
    (try (Double/parseDouble v) (catch Exception _ 0))
    v))

(defn- adjust-slice-params [params sound-name]
  (if-let [slice (get @samples/sample-slices sound-name)]
    (let [s-begin (:begin slice)
          s-end (:end slice)
          s-dur (- s-end s-begin)
          p-begin (get params :begin 0)
          p-end (get params :end 1)]
      (assoc params
             :begin (+ s-begin (* p-begin s-dur))
             :end (+ s-begin (* p-end s-dur))))
    params))

(defn- calculate-sustain [params sample-buf dur-beats monophonic?]
  (let [legato (try-parse-number (get params :legato 1.0))
        step-dur-sec (* dur-beats (/ 60 (ov/metro-bpm metro)) legato)
        param-sustain (:sustain params)
        env (get params :env :adsr)]
    (cond
      ;; For monophonic ADSR, we want the synth to stay alive indefinitely
      ;; until gated off by the next note or the loop stopping.
      (and monophonic? (= env :adsr))
      9999

      param-sustain
      (if (string? param-sustain)
        (try (Double/parseDouble param-sustain) (catch Exception _ 0.1))
        param-sustain)

      (and sample-buf (:end params))
      (let [b (get params :begin 0)
            e (:end params)
            r (get params :rate 1)
            abs-r (abs (double r))
            dur (:duration sample-buf)
            total-dur (* (abs (double (- e b))) dur (/ 1 (max 0.001 abs-r)))
            total-dur (min total-dur step-dur-sec)]
        (if (= env :perc)
          (let [attack (let [a (get params :attack 0)]
                         (if (string? a) (try (Double/parseDouble a) (catch Exception _ 0)) a))]
            (max 0.001 (- total-dur attack)))
          (let [release (let [r (get params :release 0)]
                          (if (string? r) (try (Double/parseDouble r) (catch Exception _ 0)) r))]
            (max 0.001 (- total-dur release)))))

      :else
      step-dur-sec)))

(defn at-metro
  "Executes synth-var with args at the specified beat.
   Uses Overtone's 'at' for sample-accurate timing."
  [beat synth-var args]
  (ov/at (metro beat) (apply synth-var args)))

(defn gate-off [inst]
  (when (and inst (ov/node-active? inst))
    (try
      (ov/ctl inst :gate 0)
      (catch Exception _ nil))))

(defn update-mono-inst [inst args]
  (apply ov/ctl inst args))

(defn start-mono-inst [key voice-idx synth-var args old-inst]
  (gate-off old-inst)
  (let [new-inst (apply synth-var args)]
    (swap! player-state assoc-in [:active-synths [key voice-idx]]
           {:inst new-inst :synth synth-var})))

(defn at-metro-mono [beat key voice-idx synth-var args]
  (ov/apply-at (metro beat)
               (fn [& _]
                 (when (contains? (:loops @player-state) key)
                   (let [existing (get-in @player-state [:active-synths [key voice-idx]])
                         inst (:inst existing)]
                     (if (and inst (ov/node-active? inst) (= (:synth existing) synth-var))
                       (update-mono-inst inst args)
                       (start-mono-inst key voice-idx synth-var args inst)))))))

(defn trigger-single-event [key ev params beat dur-beats voice-idx]
  (let [sound-param (:sound params)
        n-raw (:note params)
        degree (or (get params :degree) 0)
        ;; Resolve actual MIDI note by adding degree interval to root note
        n (when n-raw
            (if (number? n-raw)
              (+ (double n-raw) (double degree))
              (let [root (ov/note n-raw)]
                (+ (double root) (double degree)))))
        sound-name (or sound-param (if n :saw nil))
        slice (when sound-name (get @samples/sample-slices sound-name))
        effective-sound (if slice (:source slice) sound-name)
        sample-buf (when effective-sound (get @samples/samples effective-sound))
        params (adjust-slice-params params sound-name)
        note-offset (or (get params :add) 0)
        amp (try-parse-number (or (:amp params) 1.0))
        lpf (try-parse-number (or (:lpf params) 2000))
        slide (try-parse-number (or (get params :slide) 0))
        cycle-sec (* 4 (/ 60 (or (ov/metro-bpm metro) 120)))
        monophonic-param (is-active? (or (get params :monophonic) 0))]
    (when sound-name
      (let [base (if sample-buf :sampler (get synths/synth-aliases sound-name sound-name))
            monophonic (and monophonic-param (synths/supports-mono? base))
            sustain-sec (calculate-sustain params sample-buf dur-beats monophonic)
            synth-key (synths/get-synth-name base params)
            synth-var (or (synths/resolve-synth synth-key) (synths/resolve-synth base))
            freq (when (and n (not (p/is-rest? n-raw)))
                   (resolve-note (+ (double n) (double note-offset))))
            default-freq (if (synths/percussive-synths base) 65.406 440.0)
            effective-freq (or freq default-freq)
            last-f (get-in @player-state [:last-freq [key voice-idx]])
            has-glide (and monophonic (pos? (double slide)) last-f freq)
            effective-slide-from (if has-glide last-f effective-freq)
            effective-slide-time (if has-glide (min sustain-sec (* (double slide) cycle-sec)) 0.001)
            default-env (if (synths/percussive-synths base) :perc :adsr)
            env-param (get params :env default-env)
            env-type (if (= env-param :perc) 1 0)
            reserved #{:sound :note :degree :active :start :duration :env :add :swing :slide :legato :monophonic :gate}
            handled #{:amp :lpf :sustain :freq :slide-from :gate :monophonic :env-type}
            args (cond-> (reduce-kv (fn [acc k v] (if (or (reserved k) (handled k)) acc (conj acc k v))) [] params)
                   true (conj :amp amp)
                   true (conj :freq effective-freq)
                   lpf (conj :lpf lpf)
                   sustain-sec (conj :sustain sustain-sec)
                   true (conj :slide effective-slide-time)
                   true (conj :slide-from effective-slide-from)
                   true (conj :gate 1)
                   true (conj :monophonic (if monophonic 1 0))
                   true (conj :env-type env-type)
                   sample-buf (conj :buf (:id sample-buf)))
            ;; Filter out any nils (e.g. if lpf or sample-buf was nil)
            args (->> (partition 2 args)
                      (filter (fn [[k v]] (and (some? k) (some? v))))
                      (apply concat)
                      vec)]
        (when freq (swap! player-state assoc-in [:last-freq [key voice-idx]] freq))
        (when synth-var
          (let [effective-note (when (and n (not (p/is-rest? n-raw)))
                                 (+ (double n) (double note-offset)))
                log-data (cond-> (assoc (into {} ev) :params (assoc params :note n))
                           effective-note (assoc :effective-note effective-note)
                           effective-freq (assoc :effective-freq effective-freq))]
            (ov/apply-at (metro beat) (fn [& _] (tel/log! :info {:event log-data})))
            (if monophonic
              (at-metro-mono beat key voice-idx synth-var args)
              (do
                ;; If we were monophonic but now polyphonic, gate off the old synth
                (when-let [existing (get-in @player-state [:active-synths [key voice-idx]])]
                  (ov/apply-at (metro beat)
                               (fn [& _]
                                 (gate-off (:inst existing))
                                 (swap! player-state update :active-synths dissoc [key voice-idx]))))
                (at-metro beat synth-var args)))))))))

(defn trigger-event
  ([key ev beat dur-beats] (trigger-event key ev beat dur-beats 0 0))
  ([key ev beat dur-beats voice-idx] (trigger-event key ev beat dur-beats voice-idx 0))
  ([key ev beat dur-beats voice-idx cycle]
   (try
     (let [raw-params (:params ev)
           ;; Use source-time if available to keep random params stable across ribbon loops
           param-beat (get ev :source-time beat)
           params (resolve-params raw-params param-beat cycle)
           active? (and (is-active? (get params :active 1))
                        (not (p/is-rest? (:note params)))
                        (not (p/is-rest? (:sound params))))]
       (if active?
         (let [n (:note params)]
           (cond
             (and (sequential? n) (not (string? n)))
             (doseq [[idx note] (map-indexed vector n)]
               (trigger-event key (assoc-in ev [:params :note] note)
                              beat dur-beats (+ voice-idx idx) cycle))

             (set? n)
             (doseq [[idx note] (map-indexed vector n)]
               (trigger-event key (assoc-in ev [:params :note] note)
                              beat dur-beats (+ voice-idx idx) cycle))

             :else
             (trigger-single-event key ev params beat dur-beats voice-idx)))
         ;; Deactivated event: If it was monophonic, we should gate off any existing instances
         (let [active-synths (:active-synths @player-state)]
           (doseq [[k active] active-synths]
             ;; If top-level voice-idx is 0, we gate off ALL voices for this key.
             ;; Otherwise we gate off the specific voice (and any nested voices).
             (when (and (vector? k) (= (first k) key) (or (zero? voice-idx) (>= (second k) voice-idx)))
               (ov/apply-at (metro beat)
                            (fn [& _]
                              (gate-off (:inst active))
                              (swap! player-state update :active-synths dissoc k))))))))
     (catch Exception e
       (tel/log! :error {:msg "Error triggering event"
                         :error (ex-message e)
                         :event ev})))))

(defn apply-swing [t amount step-size]
  (let [step-idx (long (/ t step-size))]
    (if (odd? step-idx)
      (+ t (* amount step-size))
      t)))

(defn cycle-n
  "cycle the collection and take n from it"
  [n col]
  (take n (cycle col)))

(defn- stop-pattern! [key]
  (let [active-synths (:active-synths @player-state)]
    (doseq [[k active] active-synths]
      (let [k-base (if (vector? k) (first k) k)]
        (when (= k-base key)
          (gate-off (:inst active))
          (swap! player-state update :active-synths dissoc k))))
    (swap! player-state (fn [s]
                          (-> s
                              (update :patterns dissoc key)
                              (update :loops disj key))))))

(defn- schedule-cycle-events [key beat cycle-dur pat cycle-total]
  (let [events (:events pat)
        num-cycles (or (:length pat)
                       (if (seq events)
                         (inc (long (apply max (map :time events))))
                         1))
        num-iterations (if (< num-cycles 1) (long (/ 1 num-cycles)) 1)
        min-dur (if (seq events) (apply min (map :duration events)) 0.125)]
    (doseq [i (range num-iterations)]
      (let [cycle-idx (mod (+ cycle-total i) num-cycles)
            events-by-time (group-by :time events)]
        (doseq [[t evs] events-by-time]
          (when (and (>= t cycle-idx) (< t (min (+ cycle-idx 1.0) num-cycles)))
            (let [rel-t (- t cycle-idx)
                  rel-t-iter (+ rel-t (* i num-cycles))]
              (doseq [[vidx ev] (map-indexed vector evs)]
                (let [swing-raw (get-in ev [:params :swing] 0)
                      swing-fn (if (fn? swing-raw) swing-raw (constantly swing-raw))
                      swing-amount (swing-fn beat :swing)
                      swung-start (if (zero? swing-amount)
                                    rel-t-iter
                                    (apply-swing rel-t-iter swing-amount min-dur))
                      ev (assoc ev :effective-time swung-start)
                      rel-dur (:duration ev)
                      ev-beat (+ beat (* swung-start cycle-dur))
                      ev-dur-beats (* rel-dur cycle-dur)]
                  (trigger-event key ev ev-beat ev-dur-beats vidx cycle-total)))

              (let [first-ev (first evs)
                    raw-mono (get-in first-ev [:params :monophonic] 0)
                    mono-fn (if (fn? raw-mono) raw-mono (constantly raw-mono))
                    monophonic (is-active? (mono-fn beat :monophonic))]
                (when monophonic
                  (let [num-voices (count evs)
                        active-synths (:active-synths @player-state)]
                    (doseq [[k active] active-synths]
                      (when (and (vector? k)
                                 (= (first k) key)
                                 (>= (second k) num-voices))
                        (ov/apply-at (metro beat)
                                     (fn [& _]
                                       (gate-off (:inst active))
                                       (swap! player-state update
                                              :active-synths dissoc k)))))))))))))))

(defn play-loop [key beat start-beat]
  (let [state @player-state]
    (if (and (:playing? state) (contains? (:loops state) key))
      (let [pat (get-in state [:patterns key])]
        (if pat
          (let [next-beat-atom (atom nil)]
            (try
              (let [stop-cycles (:stop-cycles pat)
                    elapsed-cycles (/ (- beat start-beat) 4.0)]
                (if (and stop-cycles (>= (+ elapsed-cycles 0.001) stop-cycles))
                  (stop-pattern! key)
                  (let [cycles-raw (:cycles pat (constantly 1))
                        cycles-fn (if (fn? cycles-raw)
                                    cycles-raw
                                    (constantly cycles-raw))
                        cycles (cycles-fn beat :cycles)
                        cycle-dur (/ 4 (if (pos? cycles) cycles 1))
                        next-beat (+ beat cycle-dur)
                        cycle-total (Math/round (double (/ beat cycle-dur)))]
                    (reset! next-beat-atom next-beat)
                    (schedule-cycle-events key beat cycle-dur pat cycle-total))))
              (catch Exception e
                (tel/log! :error {:msg "Error in play-loop"
                                  :error (ex-message e)})))

            (let [nb (or @next-beat-atom (+ beat 4))]
              (when (and (:playing? @player-state)
                         (contains? (:loops @player-state) key))
                (ov/apply-by (metro nb) #'play-loop [key nb start-beat]))))
          (stop-pattern! key)))
      (stop-pattern! key))))

(defn playing
  "Returns a list of the names of the currently playing patterns.
  nil if nothing is playing"
  []
  (seq (:loops @player-state)))

(defn play!
  [& args]
  (let [pairs (if (= 1 (count args))
                [[:main (first args)]]
                (partition 2 args))
        something-playing? (seq (playing))]
    (let [quant 4]
      (doseq [[key pattern] pairs]
        (let [start-loop? (not (contains? (:loops @player-state) key))]
          (swap! player-state (fn [s]
                                (-> s
                                    (assoc :playing? true)
                                    (assoc-in [:patterns key] pattern)
                                    (update :loops conj key))))
          (when start-loop?
            (let [now (metro)
                  delay-beats (* (get pattern :delay-cycles 0) 4)
                  start-beat (+ now (- quant (mod now quant)) delay-beats)]
              (ov/apply-by (metro start-beat) #'play-loop [key start-beat start-beat])))))
      (map first pairs))))

(defn play-only!
  "Like play!, but stops any other patterns that are currently playing."
  [& args]
  (let [pairs (if (= 1 (count args))
                [[:main (first args)]]
                (partition 2 args))
        new-keys (set (map first pairs))
        active-synths (:active-synths @player-state)]
    ;; Gate off synths that are being removed
    (doseq [[k active] active-synths]
      (let [k-base (if (vector? k) (first k) k)]
        (when-not (new-keys k-base)
          (gate-off (:inst active))
          (swap! player-state update :active-synths dissoc k))))
    (swap! player-state update :patterns select-keys new-keys)
    (apply play! args)))

(defn stop!
  ([]
   (swap! player-state assoc :playing? false :patterns {} :loops #{}))
  ([key]
   (stop-pattern! key)))
