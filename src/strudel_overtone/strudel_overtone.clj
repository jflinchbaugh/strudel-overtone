(ns strudel-overtone.strudel-overtone
  (:require [overtone.core :as ov :refer :all :exclude [note lpf]]
            [clojure.string :as str]))

;; --- Synths ---

(defsynth kick [amp 1 sustain 0.3 freq 60]
  (let [env (env-gen (perc 0.01 sustain) :action FREE)
        snd (sin-osc (line:kr (* 2 freq) freq 0.1))]
    (out 0 (pan2 (* snd env amp) 0))))

(defsynth snare [amp 1 sustain 0.2 freq 200]
  (let [env (env-gen (perc 0.01 sustain) :action FREE)
        ;;tone (sin-osc freq)
        noise (ov/lpf (white-noise) 3000)
        snd (+ (* 0.5 #_tone) (* 0.8 noise))]
    (out 0 (pan2 (* snd env amp) 0))))

(defsynth saw-synth [freq 440 amp 1 sustain 0.2 cutoff 2000 resonance 0.1 pan 0
                     attack 0.01 decay 0.1 s-level 0.5 release 0.3]
  (let [env (env-gen (adsr attack decay s-level release)
                     :gate (line:kr 1 0 sustain)
                     :action FREE)
        snd (saw freq)
        filt (rlpf snd cutoff resonance)]
    (out 0 (pan2 (* filt env amp) pan))))

;; --- Pattern Engine ---

(defrecord Event [time duration params])
(defrecord Pattern [events cycles])

(defn make-pattern [events]
  (->Pattern events 1))

(defn parse-mini
  "Naively parses a space-separated string into a sequence of events with duration.
   Returns a list of maps {:value v :start s :duration d}."
  [s]
  (let [tokens (str/split (str/trim s) #"\s+")
        n (count tokens)
        dur (/ 1.0 n)]
    (map-indexed (fn [i v]
                   {:value v
                    :start (* i dur)
                    :duration dur})
                 tokens)))

(defn with-param
  "Updates pattern events with a specific parameter."
  [pattern key value]
  (update pattern :events
          (fn [evs]
            (map (fn [e] (assoc-in e [:params key] value)) evs))))

(defn s
  "Creates a pattern from a sound string (mini-notation), or sets the sound of an existing pattern."
  ([pat-str]
   (let [parsed (parse-mini pat-str)
         events (keep (fn [p]
                        (when (not= "_" (:value p))
                          (->Event (:start p)
                                   (:duration p)
                                   {:sound (:value p)})))
                      parsed)]
     (make-pattern events)))
  ([pattern sound-val]
   (with-param pattern :sound sound-val)))

(defn note
  "Creates a pattern from a note string (mini-notation), or sets the note of an existing pattern."
  ([pat-str]
   (let [parsed (parse-mini pat-str)
         events (keep (fn [p]
                        (when (not= "_" (:value p))
                          (->Event (:start p)
                                   (:duration p)
                                   {:note (:value p)})))
                      parsed)]
     (make-pattern events)))
  ([pattern note-val]
   (with-param pattern :note note-val)))

(defn gain [pattern val]
  (with-param pattern :amp val))

(defn lpf [pattern val]
  (with-param pattern :cutoff val))

(defn fast [pattern amount]
  (update pattern :cycles #(* % amount)))

;; --- Player ---

(defonce metro (metronome 120))
(defonce player-state (atom {:playing? false :patterns {} :loops #{}}))

(defn- resolve-note [n]
  (if (number? n) n (ov/midi->hz (ov/note n))))

(defn- trigger-event [ev beat dur-beats]
  (let [params (:params ev)
        sound (:sound params)
        n (:note params)
        amp (or (:amp params) 1.0)
        cutoff (or (:cutoff params) 2000)
        ;; Calculate sustain in seconds from beats
        sustain-sec (* dur-beats (/ 60 (metro-bpm metro)))
        ;; Default sound if only note is provided
        sound (or sound (if n "saw-synth" nil))]

    (when sound
      (let [synth-fn (case sound
                       "bd" kick
                       "sd" snare
                       "saw-synth" saw-synth
                       nil)
            freq (if n (resolve-note n) nil)
            args (cond-> [:amp amp]
                   freq (conj :freq freq)
                   cutoff (conj :cutoff cutoff)
                   sustain-sec (conj :sustain sustain-sec))]
        (when synth-fn
          (apply-at (metro beat) synth-fn args))))))

(defn- play-loop [key beat]
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

            (apply-at (metro next-beat) #'play-loop [key next-beat]))
          ;; Pattern removed, loop dies
          (swap! player-state update :loops disj key)))
      ;; Stopped, loop dies
      (swap! player-state update :loops disj key))))
(defn play!
  ([pattern] (play! :main pattern))
  ([key pattern]
   (let [start-loop? (not (contains? (:loops @player-state) key))]
     (swap! player-state (fn [s]
                           (-> s
                               (assoc :playing? true)
                               (assoc-in [:patterns key] pattern)
                               (update :loops conj key))))
     (when start-loop?
       (let [now (metro)
             start-beat (+ now (- 4 (mod now 4)))]
         (apply-at (metro start-beat) #'play-loop [key start-beat]))))))

(defn stop!
  ([] (swap! player-state assoc :playing? false))
  ([key] (swap! player-state update :patterns dissoc key)))

;; --- Main / Entry ---

(defn -main [& args]
  (connect-server)
  (println "Strudel-Overtone Ready."))


(comment

  (connect-server)

  ;; Play a bassline
  (play! :bass (-> (note "c2 g2") (s "saw-synth")))

  ;; Layer drums on top (aligned)
  (play! :snare (s "sd sd sd sd sd sd sd sd"))
  (play! :bd (s "bd _ bd _"))

  ;; Update bassline
  (play! :bass (-> (note "c2 e2 g2 b2") (s "saw-synth")))

  ;; Stop just the drums
  (stop! :drums)

  ;; Stop everything
  (stop!)

  .)
