(ns strudel-overtone.degrees-demo
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

(comment
  (cpm 30)

  ;; A simple melodic demo using degrees
  (let [base-note :c2
        ;; A bassline walking through minor degrees
        bass (-> (note base-note)
                 (degrees :major [1 1 4 5])
                 (s :saw)
                 (fast 1/2)
                 (gain 3/10)
                 (lpf 100)
                 (legato 0.8))

        ;; A lead melody using a slightly more complex degree pattern
        lead (-> (note base-note)
                 (add 24)
                 (s :supersaw)
                 #_(swing 1/10)
                 (degrees :major [1 3 4 5 7 6 7 8])
                 (gain 5/5)
                 (pan (sine 4/30 1 -1))
                 (lpf (sine 1/60 400 4000))
                 (echo 0.375 0.6))

        ;; Some drums to keep time
        drums (-> (s [:kick :- :- :-])
                  (gain 4/10)
                  (slow 2)
                  #_(lpf 400))

        hat (-> (s [[:hat :hat] :- :hat :-])
                  (gain 1/10)
                  (legato 1/2)
                  (slow 1)
                  (lpf (sine 1/30 800 5000)))]

    (play-only!
     :bass bass
     :lead lead
     :drums drums
     :hat hat))

  (stop!)

  .)
