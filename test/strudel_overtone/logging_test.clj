(ns strudel-overtone.logging-test
  (:require [clojure.test :refer [deftest is testing]]
            [strudel-overtone.core :as sut]
            [strudel-overtone.player :as player]
            [strudel-overtone.synths :as synths]
            [overtone.core :as ov]
            [taoensso.telemere :as tel]
            [strudel-overtone.samples :as samples]))

(deftest sample-logging-test
  (testing "sample loading and slicing logs"
    (let [samples-atom (atom {})
          slices-atom (atom {})]
      (with-redefs [samples/samples samples-atom
                    samples/sample-slices slices-atom
                    ov/load-sample (fn [p] (if (= p "valid.wav") {:id 1 :duration 1.0} (throw (Exception. "file not found"))))
                    ov/freesound (fn [id] (if (= id 123) {:id 2 :duration 1.0} (throw (Exception. "not found"))))]

        (testing "load-sample! success"
          (let [signals (:signals (tel/with-signals (samples/load-sample! :kick "valid.wav")))
                log (first (filter #(= :info (:level %)) signals))]
            (is (= {:sample-loaded {:name :kick :from-path "valid.wav"}} (:msg_ log)))))

        (testing "load-sample! failure"
          ;; One error log from catch, one warn log from final nil check
          (let [signals (:signals (tel/with-signals (samples/load-sample! :kick "missing.wav")))
                warn-log (first (filter #(= :warn (:level %)) signals))]
            (is (= {:sample-not-loaded {:name :kick :from-path "missing.wav"}} (:msg_ warn-log)))))

        (testing "load-freesound! success"
          (let [signals (:signals (tel/with-signals (samples/load-freesound! :snare 123)))
                log (first (filter #(= :info (:level %)) signals))]
            (is (= {:freesound-loaded {:name :snare :id 123}} (:msg_ log)))))

        (testing "slice-sample! logging"
          (let [signals (:signals (tel/with-signals (samples/slice-sample! :k :kick 0 0.1)))
                log (first signals)]
            (is (= {:sample-sliced {:name :k :from :kick}} (:msg_ log)))))

        (testing "slice-sample-ms! failure"
          (let [signals (:signals (tel/with-signals (samples/slice-sample-ms! :k :missing 0 100)))
                log (first signals)]
            (is (= {:slice-ms-failed {:name :k :source :missing :reason "Source sample not found"}} (:msg_ log)))))))))

(deftest logging-values-test
  (testing "logged event contains calculated values"
    (let [mock-metro (fn [& args]
                       (if (number? (first args))
                         (+ 1000 (first args))
                         120))
          player-state (atom {:playing? true
                              :patterns {:test (sut/s [:c4])}
                              :loops #{:test}})]

      (with-redefs [player/metro mock-metro
                    player/player-state player-state
                    player/at-metro (fn [& _])
                    player/at-metro-mono (fn [& _])
                    ov/apply-at (fn [_ f] (f)) ;; Execute immediately
                    ov/apply-by (fn [_ f args] (apply f args))
                    ov/metro-bpm (constantly 120)]

        (testing "trigger-single-event logging"
          (let [ev {:time 0.0 :duration 1.0 :params {:note :c4}}
                params {:note 60.0 :amp 1.0}
                beat 1000
                dur-beats 4.0
                signals (:signals (tel/with-signals (player/trigger-single-event :test ev params beat dur-beats 0)))
                log-signal (first (filter #(= :info (:level %)) signals))
                log-data (:msg_ log-signal)]
            (is (= :info (:level log-signal)))
            (is (contains? log-data :event))
            (let [event-log (:event log-data)]
              (is (= 60.0 (:effective-note event-log)))
              (is (= (ov/midi->hz 60.0) (:effective-freq event-log)))
              (is (= 60.0 (get-in event-log [:params :note]))))))

        (testing "trigger-single-event with degree and add"
          (let [ev {:time 0.0 :duration 1.0 :params {:note :c4 :degree 7 :add 12}}
                params {:note 67.0 :add 12 :amp 1.0}
                beat 1000
                dur-beats 4.0
                signals (:signals (tel/with-signals (player/trigger-single-event :test ev params beat dur-beats 0)))
                event-log (:event (:msg_ (first (filter #(= :info (:level %)) signals))))]
            (is (= 79.0 (:effective-note event-log)))
            (is (= (ov/midi->hz 79.0) (:effective-freq event-log)))))

        (testing "schedule-cycle-events sets effective-time"
          (let [pat (sut/note [:c4 :d4])
                cycle-dur 4.0
                signals (:signals (tel/with-signals (#'player/schedule-cycle-events :test 1000 cycle-dur pat 0)))
                info-signals (filter #(= :info (:level %)) signals)
                event-logs (map (comp :event :msg_) info-signals)
                ev1 (first event-logs)
                ev2 (second event-logs)]
            (is (= 0.0 (:effective-time ev1)))
            (is (= 0.5 (:effective-time ev2)))))

        (testing "cpm logging"
          (let [signals (:signals (tel/with-signals (player/cpm 140)))
                log-signal (first signals)]
            (is (= :info (:level log-signal)))
            (is (= {:cpm 140} (:msg_ log-signal)))))

        (testing "trigger-event error logging"
          (let [ev {:time 0.0 :params {:note :c4}}
                signals (:signals (tel/with-signals
                                   (with-redefs [player/resolve-params (fn [& _] (throw (Exception. "resolve error")))]
                                     (player/trigger-event :test ev 1000 4.0))))
                log-signal (first signals)]
            (is (= :error (:level log-signal)))
            (is (= "Error triggering event" (get-in log-signal [:msg_ :msg])))
            (is (= "resolve error" (get-in log-signal [:msg_ :error])))))))))
