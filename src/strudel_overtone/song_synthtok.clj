(ns strudel-overtone.song-synthtok
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

(def lpf-a (atom 100))

(let [bp [:b2]
      bi [4 1 1 1 2 2 1 1]
      g [0.6]
      lpf-v 200]
  (cpm 174/8)
  (play-only!
   :b1 (-> (s :saw)
           (note bp)
           (degrees :minor bi)
           (gain g)
           (legato 1/2)
           (lpf 300)
           (lpf-env 1000)
           (lpf-adsr 0.01 0.2 0.0 0))
   :b2 (-> (s :sine)
           (note bp)
           (degrees :minor bi)
           (legato 1/2)
           (add -12)
           (gain g)
           (gain 2)
           (lpf 400))
   :drum (-> (s [:bd :- :- :- :- :bd :- :-])
             (lpf 150))
   :snare (-> (s [:- :- :sd :- :- :- :sd :-]))
   :clap (-> (s [:cp :cp :cp :cp :cp :cp :cp :cp])
             (gain (adsr-sig 0 0 1 1)))))

(play-only! :s (s [:bd]))

(stop!)

((adsr-sig 0 0 1 1) 0 1)

(saw 1)

(reset! lpf-a 200)

;; 1. Classic Acid 303 Bass (Filter ADSR Sweep)
(play-only! :acid (-> (note [:c2 :c2 :c2])
                      (s :tb303)
                      (lpf 500)                      ; Resting base cutoff = 300 Hz
                      (lpf-env 300)                    ; Peak envelope sweep depth = 5000 Hz
                      (lpf-adsr 0.01 0.1 0.8 0.2)   ; Plucky filter sweep (att dec sus rel)
                      (adsr 0.01 0.2 0.8 0.1)        ; Volume envelope
                      (legato 1/2)
                      (resonance 0.2)
                      (res-env 0.6)
                      (res-adsr 0.01 0.2 0.8 0.1)))

;; 2. Downward Filter Sweep (Bright Pluck decaying to warm tone)
(play-only! :pluck (-> (note [:c3 :g3 :c4 :eb4])
                       (s :square)
                       (lpf 6000)                    ; Start bright at 6000 Hz cutoff
                       (lpf-env -5000)                  ; Sweep DOWN by 5000 Hz as envelope decays
                       (lpf-adsr 0.005 0.25 0.0 0.1) ; Fast decay to 0 sustain
                       (adsr 0.005 0.3 0.6 0.1)
                       (phaser-hz 1)
                       (phaser-depth 0)
                       (phaser-env 5)
                       (phaser-perc 0.1)))

;; 3. Slow Ambient Pad with Soft Filter Envelope
(play-only! :pad (-> (note #{:c3 :eb3 :g3 :bb3})
                     (add 12)
                     (s :saw)
                     #_(lpf 100)                       ; Dark baseline cutoff
                     #_(lpf-env 1000)                     ; Gentle 3kHz filter swell
                     #_(lpf-adsr 1.5 2.0 0.7 2.0)      ; Slow filter swell
                     #_(adsr 0.5 0 0.8 0.5)          ; Slow volume swell
                     #_(legato 0.9)))


;; Telephone / Radio Vocoder Sweep
(play-only!
 (-> (s :saw)
     (note [:c2 :e2 :g2 :b2])
     (bpf 400)               ; Base center frequency at 400 Hz
     (bpf-env 2400)          ; Envelope sweeps center freq up to +2400 Hz
     (bpf-perc 0.05)         ; Percussive envelope attack/decay
     (resonance 0.8)))        ; Sharp resonant peak for a vocal "wah" effect


(play-only! :pad (-> (note [#{:c3 :eb3} #{:c4 :eb4}])
                     (glide 1)
                     (mono)
                     (s :saw)))

(stop!)

(ov/stop)
