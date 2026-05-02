(ns strudel-overtone.degrees-demo
  (:require [strudel-overtone.core :refer :all]
            [overtone.core :as ov]))

(comment
  (cpm 30)

  ;; A simple melodic demo using degrees
  (let [base-note :c2
        ;; A bassline walking through minor degrees
        bass (-> (note base-note)
               (degrees :minor [1 1 4 5])
               (s :saw)
               (fast 2)
               (gain 0.3)
               (lpf 800)
               (legato 0.8))

        ;; A lead melody using a slightly more complex degree pattern
        lead (-> (note base-note)
               (add 12)
               (degrees :minor [1 3 4 5 4 3 7 8])
               (degrade 0.1)
               (s :supersaw)
               (gain 0.8)
               (lpf (sine 0.125 500 4000))
               (echo 0.375 0.6))

        ;; Some drums to keep time
        drums (-> (s [:kick [:- :hat] [:- :hat] [:- :snare]])
                (gain 0.5))]

    (play-only!
      :bass bass
      :lead lead
      :drums drums))

  (stop!)

 .)
