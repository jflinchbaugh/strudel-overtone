(ns strudel-overtone.song-2
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

(comment

  (cpm 160/4)

  (def-additive! :add [1 0.1 0.5])

  (play-only!
   :bass (->
          (s :fm)
          (note (choose-n 8 (chord :a0 :minor7)))
          (ribbon 1 4)
          (gain 0.5))
   :hat (->
          (s :sine)
          (gain 0.5)
          (note [:- :a0])))

  (ov/midi->hz (note->midi :g#2))

  (stop!)

  .)
