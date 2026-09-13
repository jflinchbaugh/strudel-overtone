(ns strudel-overtone.midi
  (:require [overtone.midi :as ov-midi]
            [taoensso.telemere :as tel]
            [overtone.core :as ov]))

;; --- MIDI In Parameter State ---

(defonce
  ^{:doc "Atom holding raw MIDI CC values, named mappings, pad bindings, and connected devices."}
  midi-state
  (atom {:raw-cc {}
         :mappings {}
         :pads {}
         :connected-inputs {}}))

(defn reset-midi-state!
  "Resets raw CC values, mappings, and pad bindings."
  []
  (swap! midi-state assoc
         :raw-cc {}
         :mappings {}
         :pads {}
         :connected-outputs {}
         :default-output nil))

(defn scale-cc-val
  "Scales raw MIDI CC (0-127) to [min-val, max-val] with linear or exp curve."
  [raw min-val max-val curve]
  (let [norm (/ (double raw) 127.0)
        norm (max 0.0 (min 1.0 norm))]
    (case curve
      :exp
      (let [min-v (double (if (pos? min-val) min-val 0.0001))
            max-v (double max-val)]
        (* min-v (Math/pow (/ max-v min-v) norm)))

      ;; Default :lin
      (+ min-val (* norm (- max-val min-val))))))

(defn def-midi-cc!
  "Registers a named CC mapping with scaling parameters.
   Options:
     :min     Minimum output value (default 0.0)
     :max     Maximum output value (default 1.0)
     :curve   :lin (default) or :exp
     :default Default value before any CC event is received"
  [name-key cc-num & {:keys [min max curve default]
                      :or {min 0.0 max 1.0 curve :lin default nil}}]
  (swap! midi-state assoc-in [:mappings name-key]
         {:cc cc-num
          :min (double min)
          :max (double max)
          :curve curve
          :default (if (some? default) (double default) (double min))}))

(defn set-midi-cc!
  "Manually sets the raw value (0-127) for a CC number."
  [cc-num val]
  (swap! midi-state assoc-in [:raw-cc (long cc-num)] (double val)))

(defn def-midi-pad!
  "Binds a MIDI pad (note number) to an on-fn and optional off-fn.
   Example: (def-midi-pad! 36 #(play! :kick (s :kick)))"
  ([pad-note on-fn]
   (def-midi-pad! pad-note on-fn nil))
  ([pad-note on-fn off-fn]
   (swap! midi-state assoc-in [:pads (long pad-note)]
          {:on-fn on-fn :off-fn off-fn}))
  ([name-key pad-note on-fn off-fn]
   (swap! midi-state assoc-in [:pads (long pad-note)]
          {:name name-key :on-fn on-fn :off-fn off-fn})))

(defn def-midi-pad-toggle!
  "Binds a MIDI pad to toggle between on-fn and off-fn on alternating hits."
  ([pad-note on-fn off-fn]
   (let [state-atom (atom false)]
     (def-midi-pad! pad-note
       (fn [msg]
         (let [new-state (swap! state-atom not)]
           (if new-state
             (when on-fn (on-fn msg))
             (when off-fn (off-fn msg))))))))
  ([name-key pad-note on-fn off-fn]
   (let [state-atom (atom false)]
     (def-midi-pad! name-key pad-note
       (fn [msg]
         (let [new-state (swap! state-atom not)]
           (if new-state
             (when on-fn (on-fn msg))
             (when off-fn (off-fn msg)))))
       nil))))

(defn get-midi-cc-val
  "Retrieves the current scaled value for a named mapping or CC number."
  ([key-or-cc]
   (let [state @midi-state]
     (if-let [m (get-in state [:mappings key-or-cc])]
       (let [raw (get-in state [:raw-cc (:cc m)])]
         (if (some? raw)
           (scale-cc-val raw (:min m) (:max m) (:curve m))
           (:default m)))
       (let [raw (get-in state [:raw-cc (long key-or-cc)] 0.0)]
         (scale-cc-val raw 0.0 1.0 :lin)))))
  ([key-or-cc min-val max-val curve default-val]
   (let [state @midi-state
         raw (get-in state [:raw-cc (long key-or-cc)])]
     (if (some? raw)
       (scale-cc-val raw min-val max-val curve)
       (or default-val min-val)))))

(deftype MidiCcControl [key-or-cc min-val max-val curve default-val]
  clojure.lang.IFn
  (invoke [_]
    (if (some? min-val)
      (get-midi-cc-val key-or-cc min-val max-val curve default-val)
      (get-midi-cc-val key-or-cc)))
  (invoke [this _ _]
    (.invoke this))
  (applyTo [this args]
    (.invoke this))

  clojure.lang.IDeref
  (deref [this]
    (.invoke this)))

(defn midi-cc
  "Returns a dynamic parameter control reading from MIDI CC.
   Can be passed directly into pattern parameters (e.g. lpf, gain, pan).
   Example: (lpf (midi-cc :cutoff))
            (gain (midi-cc 1 :min 0.0 :max 1.0))"
  ([key-or-cc]
   (->MidiCcControl key-or-cc nil nil nil nil))
  ([key-or-cc & {:keys [min max curve default]
                 :or {min 0.0 max 1.0 curve :lin default nil}}]
   (->MidiCcControl key-or-cc (double min) (double max) curve
                    (if (some? default) (double default) (double min)))))

(defn midi-debug?
  "Returns true if MIDI debug logging is currently enabled."
  []
  (true? (:debug? @midi-state)))

(defn midi-debug!
  "Toggles or sets MIDI debug logging for incoming MIDI events.
   When enabled, logs incoming CC numbers/values and Note numbers/velocities.
   Example: (midi-debug!)
            (midi-debug! false)"
  ([]
   (midi-debug! true))
  ([enable?]
   (swap! midi-state assoc :debug? (boolean enable?))
   (tel/log! :info {:midi-debug (boolean enable?)})
   (boolean enable?)))

(defn format-midi-debug-msg
  "Formats incoming MIDI message for logging."
  [msg]
  (let [cmd (or (:cmd msg) (:command msg))
        status (:status msg)
        chan (or (:channel msg) (:chan msg) 0)]
    (cond
      (or (= cmd :control-change) (= status :control-change))
      (let [cc (or (:data1 msg) (:note msg) (:control msg))
            val (or (:data2 msg) (:velocity msg) (:value msg))]
        {:midi-in :control-change
         :cc (long cc)
         :val (long val)
         :channel (long chan)})

      (or (= cmd :note-on) (= status :note-on))
      (let [note (or (:data1 msg) (:note msg))
            vel (or (:data2 msg) (:velocity msg))]
        (if (and (some? vel) (pos? vel))
          {:midi-in :note-on
           :note (long note)
           :vel (long vel)
           :channel (long chan)}
          {:midi-in :note-off
           :note (long note)
           :vel 0
           :channel (long chan)}))

      (or (= cmd :note-off) (= status :note-off))
      (let [note (or (:data1 msg) (:note msg))]
        {:midi-in :note-off
         :note (long note)
         :vel 0
         :channel (long chan)})

      :else
      {:midi-in (or cmd status :unknown)
       :data1 (:data1 msg)
       :data2 (:data2 msg)
       :channel (long chan)})))

(defn handle-midi-msg
  "Processes incoming MIDI message maps (CC, Note On, Note Off)."
  [msg]
  (when (midi-debug?)
    (when-let [info (format-midi-debug-msg msg)]
      (tel/log! :info info)))
  (let [cmd (or (:cmd msg) (:command msg))
        status (:status msg)]
    (cond
      (or (= cmd :control-change) (= status :control-change))
      (let [cc (or (:data1 msg) (:note msg) (:control msg))
            val (or (:data2 msg) (:velocity msg) (:value msg))]
        (when (and (some? cc) (some? val))
          (set-midi-cc! cc val)))

      (or (= cmd :note-on) (= status :note-on))
      (let [note (or (:data1 msg) (:note msg))
            vel (or (:data2 msg) (:velocity msg))]
        (if (and (some? vel) (pos? vel))
          (when-let [pad (get-in @midi-state [:pads (long note)])]
            (when-let [f (:on-fn pad)]
              (f {:note (long note) :velocity (double vel) :msg msg})))
          (when-let [pad (get-in @midi-state [:pads (long note)])]
            (when-let [f (:off-fn pad)]
              (f {:note (long note) :velocity 0.0 :msg msg})))))

      (or (= cmd :note-off) (= status :note-off))
      (let [note (or (:data1 msg) (:note msg))]
        (when-let [pad (get-in @midi-state [:pads (long note)])]
          (when-let [f (:off-fn pad)]
            (f {:note (long note) :velocity 0.0 :msg msg}))))

      :else nil)))

;; --- Hardware Connection Helpers ---

(defn midi-in-devices
  "Lists all available MIDI input devices."
  []
  (try
    (ov-midi/midi-sources)
    (catch Exception _ [])))

(defn midi-in-connect!
  "Connects a MIDI input device to the parameter handler.
   If device-name is omitted, connects to the first available MIDI input."
  ([]
   (if-let [dev (first (midi-in-devices))]
     (midi-in-connect! dev)
     (tel/log! :warn {:msg "No MIDI input devices found."})))
  ([device-or-name]
   (try
     (let [dev (if (string? device-or-name)
                 (ov-midi/midi-find-device device-or-name)
                 device-or-name)
           dev-key (or (:name dev) (str dev))
           in-inst (ov-midi/midi-in dev)]
       (ov-midi/midi-handle-events in-inst #'handle-midi-msg)
       (swap! midi-state assoc-in [:connected-inputs dev-key] in-inst)
       (tel/log! :info {:midi-in-connected dev-key})
       dev-key)
     (catch Exception e
       (tel/log! :error {:msg "Failed to connect MIDI in"
                         :error (ex-message e)})))))

(defn midi-in-disconnect!
  "Disconnects MIDI input devices."
  ([]
   (swap! midi-state assoc :connected-inputs {}))
  ([dev-key]
   (swap! midi-state update :connected-inputs dissoc dev-key)))

;; --- MIDI Out & Hardware Feedback ---

(defn midi-out-devices
  "Lists all available MIDI output devices (sinks)."
  []
  (try
    (ov-midi/midi-sinks)
    (catch Exception _ [])))

(defn midi-out-connect!
  "Connects a MIDI output device for sending feedback to hardware.
   If device-or-name is omitted, connects to the first available MIDI output."
  ([]
   (if-let [dev (first (midi-out-devices))]
     (midi-out-connect! dev)
     (tel/log! :warn {:msg "No MIDI output devices found."})))
  ([device-or-name]
   (try
     (let [dev (if (string? device-or-name)
                 (ov-midi/midi-find-device device-or-name)
                 device-or-name)
           dev-key (or (:name dev) (str dev))
           out-inst (ov-midi/midi-out dev)]
       (swap! midi-state (fn [st]
                           (-> st
                               (assoc-in [:connected-outputs dev-key] out-inst)
                               (assoc :default-output out-inst))))
       (tel/log! :info {:midi-out-connected dev-key})
       dev-key)
     (catch Exception e
       (tel/log! :error {:msg "Failed to connect MIDI out"
                         :error (ex-message e)})))))

(defn midi-out-disconnect!
  "Disconnects MIDI output devices."
  ([]
   (swap! midi-state assoc :connected-outputs {} :default-output nil))
  ([dev-key]
   (swap! midi-state
          (fn [st]
            (let [outs (dissoc (:connected-outputs st) dev-key)]
              (assoc st
                     :connected-outputs outs
                     :default-output (some-> outs first val)))))))

(defn- resolve-midi-out
  [device]
  (cond
    (nil? device)
    (or (:default-output @midi-state)
        (some-> (:connected-outputs @midi-state) first val))

    (string? device)
    (get-in @midi-state [:connected-outputs device])

    :else device))

(defn midi-send-cc!
  "Sends a MIDI Control Change (CC) message to connected MIDI out device(s).
   Also updates local raw CC state in midi-state.
   Options:
     key-or-cc CC number (0-127) or mapped keyword (e.g. :cutoff)
     val       Raw value (0-127)
     channel   MIDI channel (default 0)
     device    Specific output device or nil for default"
  ([key-or-cc val]
   (midi-send-cc! key-or-cc val 0 nil))
  ([key-or-cc val channel]
   (midi-send-cc! key-or-cc val channel nil))
  ([key-or-cc val channel device]
   (let [cc-num (if (keyword? key-or-cc)
                  (get-in @midi-state [:mappings key-or-cc :cc] key-or-cc)
                  key-or-cc)
         v (long (max 0 (min 127 (Math/round (double val)))))
         chan (long (or channel 0))]
     (when (number? cc-num)
       (set-midi-cc! cc-num v)
       (when-let [out (resolve-midi-out device)]
         (ov-midi/midi-control out (long cc-num) v chan))
       v))))

(defn midi-set-pad-light!
  "Sets a pad's light/color state via MIDI note on/off.
   pad-note     Note number (0-127)
   color-or-vel Color index, velocity (0-127), or boolean (false=off, true=127)
   channel      MIDI channel (default 0)
   device       Specific output device or nil for default"
  ([pad-note color-or-vel]
   (midi-set-pad-light! pad-note color-or-vel 0 nil))
  ([pad-note color-or-vel channel]
   (midi-set-pad-light! pad-note color-or-vel channel nil))
  ([pad-note color-or-vel channel device]
   (when-let [out (resolve-midi-out device)]
     (let [chan (long (or channel 0))
           pad (long pad-note)]
       (cond
         (or (false? color-or-vel) (nil? color-or-vel))
         (ov-midi/midi-note-off out pad chan)

         (true? color-or-vel)
         (ov-midi/midi-note-on out pad 127 chan)

         (number? color-or-vel)
         (let [vel (long (max 0 (min 127 (Math/round (double color-or-vel)))))]
           (if (zero? vel)
             (ov-midi/midi-note-off out pad chan)
             (ov-midi/midi-note-on out pad vel chan))))))))

(def
  ^{:doc "Map of standard pad color keywords to MIDI velocity index values."}
  colors
  {:black 0
   :white 8
   :yellow 24
   :cyan 40
   :purple 56
   :blue 72
   :green 88
   :red 104})

(def color-palette
  {0   [0.0 0.0 0.0]
   8   [1.0 1.0 1.0]
   24  [1.0 1.0 0.0]
   40  [0.0 1.0 1.0]
   56  [1.0 0.0 1.0]
   72  [0.0 0.0 1.0]
   88  [0.0 1.0 0.0]
   104 [1.0 0.0 0.0]})

(defn- normalize-color-comp
  [v max-v]
  (let [d (double v)]
    (if (pos? max-v)
      (max 0.0 (min 1.0 (/ d max-v)))
      (max 0.0 (min 1.0 d)))))

(defn rgb
  "Builds a pad color index from red, green, and blue components.
   Components can be 0.0-1.0 floats or 0-255 integers.
   Returns the closest matching hardware velocity / color value."
  ([rgb-coll]
   (apply rgb rgb-coll))
  ([r g b]
   (if (and (zero? r) (zero? g) (zero? b))
     0
     (let [max-input (max (double r) (double g) (double b))
           scale (if (> max-input 1.0) 255.0 1.0)
           nr (normalize-color-comp r scale)
           ng (normalize-color-comp g scale)
           nb (normalize-color-comp b scale)
           dist-sq (fn [[_ [pr pg pb]]]
                     (let [dr (- nr pr)
                           dg (- ng pg)
                           db (- nb pb)]
                       (+ (* dr dr) (* dg dg) (* db db))))]
       (key (apply min-key dist-sq color-palette))))))

(defn coord->note
  "Converts an 8x8 grid [row col] coordinate to MIDI note number."
  [row col]
  (+ (* 16 row) col))

(defn note->coord
  "Converts a MIDI note number to an 8x8 grid [row col] coordinate."
  [note]
  [(int (/ note 16)) (mod note 16)])

(defn light-on!
  "Turns on a pad light with a named color or velocity index.
   pad can be a note number or [row col] coordinate vector."
  ([pad color]
   (let [note (if (coll? pad) (apply coord->note pad) pad)
         vel (get colors color color)]
     (midi-set-pad-light! note 0)
     (midi-set-pad-light! note vel))))

(defn light-grid!
  "Light up the whole grid with colors determined
  by calling (color-fn row column)."
  [color-fn]
  (doseq [r (range 8)
          c (range 8)]
    (light-on! [r c] (color-fn r c))))

(defn random-lights
  "Returns a random non-black color keyword from the colors map."
  [r c]
  (first (shuffle (rest (keys colors)))))

(defn test-midi-out!
  "Tests writing values to knobs and setting pad lights on MIDI hardware.
   Options:
     :ccs      Collection of CC numbers to test (default [1 2 3 4 5 6 7 8])
     :pads     Collection of pad note numbers to test (default (range 36 44))
     :steps    Number of sweep steps for CCs (default 8)
     :delay-ms Delay between steps in ms (default 30)
     :channel  MIDI channel (default 0)"
  [& {:keys [ccs pads steps delay-ms channel]
      :or {ccs [1 2 3 4 5 6 7 8]
           pads (vec (range 36 44))
           steps 8
           delay-ms 30
           channel 0}}]
  (let [step-delay (long delay-ms)]
    (doseq [cc ccs]
      (doseq [i (range (inc steps))]
        (let [val (int (* 127 (/ i (double steps))))]
          (midi-send-cc! cc val channel)
          (Thread/sleep step-delay)))
      (doseq [i (range steps -1 -1)]
        (let [val (int (* 127 (/ i (double steps))))]
          (midi-send-cc! cc val channel)
          (Thread/sleep step-delay))))
    (doseq [p pads]
      (doseq [vel [32 64 96 127 0]]
        (midi-set-pad-light! p vel channel)
        (Thread/sleep step-delay)))
    (doseq [p pads]
      (midi-set-pad-light! p 0 channel))
    :ok))
