(ns strudel-overtone.strudel-overtone
  (:require [overtone.core :refer :all])
  (:gen-class))

(connect-server)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (demo (sin-osc (midi->hz 70))))

(comment


  (->
    70
    midi->hz
    saw
    demo)



  .)
