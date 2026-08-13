(ns strudel-overtone.song-test-ducking
  (:require  [overtone.core :as ov]
             [strudel-overtone.core :refer :all]))

(comment
  ;; Test 1: Loud continuous synth with ducking enabled
  (play-only! :pad (-> (note :c4)
                       (s :saw)
                       (mono)
                       (duck 1.0))
              ;; Silent trigger firing on every beat with exaggerated 0.8s release
              :duck (-> (s [:bd [:bd :bd] :bd :bd])
                        (gain 0.0)
                        (duck-trigger 1.0)
                        (duck-attack 0.01)
                        (duck-release 0.8)))

  (stop!)

  (ov/stop)

  (strudel-overtone.synths/get-duck-bus)


  (def bus-mon (ov/bus-monitor  (strudel-overtone.synths/get-duck-bus)))

  (deref bus-mon)

  )
