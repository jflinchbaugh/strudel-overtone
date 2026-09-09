(ns strudel-overtone.midi-test
  (:require [clojure.test :refer [deftest is testing]]
            [strudel-overtone.midi :as midi]
            [strudel-overtone.pattern :as p]
            [strudel-overtone.player :as player]))

(deftest midi-scaling-test
  (testing "linear scaling"
    (is (== 0.0 (midi/scale-cc-val 0 0.0 1.0 :lin)))
    (is (== 1.0 (midi/scale-cc-val 127 0.0 1.0 :lin)))
    (is (== 50.0 (midi/scale-cc-val 63.5 0.0 100.0 :lin))))

  (testing "exponential scaling"
    (is (== 200.0 (midi/scale-cc-val 0 200.0 8000.0 :exp)))
    (is (== 8000.0 (midi/scale-cc-val 127 200.0 8000.0 :exp)))))

(deftest midi-cc-definition-and-resolution-test
  (testing "defining and updating named MIDI CC"
    (midi/reset-midi-state!)
    (midi/def-midi-cc! :cutoff 74 :min 200.0 :max 8000.0 :curve :exp :default 200.0)

    ;; Before any MIDI message arrives, default is returned
    (is (== 200.0 (midi/get-midi-cc-val :cutoff)))

    ;; Incoming MIDI CC event update
    (midi/handle-midi-msg {:cmd :control-change :data1 74 :data2 127})
    (is (== 8000.0 (midi/get-midi-cc-val :cutoff)))

    (midi/handle-midi-msg {:cmd :control-change :data1 74 :data2 0})
    (is (== 200.0 (midi/get-midi-cc-val :cutoff)))))

(deftest midi-cc-pattern-integration-test
  (testing "midi-cc function in pattern parameter resolution"
    (midi/reset-midi-state!)
    (midi/def-midi-cc! :res 71 :min 0.0 :max 1.0 :curve :lin :default 0.2)

    (let [pat (-> (p/note [:c3])
                  (p/s :saw)
                  (p/resonance (midi/midi-cc :res)))
          ev (first (:events pat))]
      ;; Resolves to default 0.2
      (is (== 0.2 (:resonance (player/resolve-params (:params ev) 0 0))))

      ;; Update CC 71 to value 127 (max 1.0)
      (midi/handle-midi-msg {:cmd :control-change :data1 71 :data2 127})
      (is (== 1.0 (:resonance (player/resolve-params (:params ev) 0 0))))

      ;; Direct deref works
      (is (== 1.0 @(midi/midi-cc :res))))))

(deftest inline-midi-cc-test
  (testing "inline midi-cc by CC number without explicit def-midi-cc!"
    (midi/reset-midi-state!)
    (let [ctrl (midi/midi-cc 1 :min 0.0 :max 10.0)]
      (is (== 0.0 (ctrl 0 :gain)))
      (midi/set-midi-cc! 1 127)
      (is (== 10.0 (ctrl 0 :gain)))
      (is (== 10.0 @ctrl)))))

(deftest midi-pad-test
  (testing "binding MIDI pads to trigger functions"
    (midi/reset-midi-state!)
    (let [pad-hit (atom nil)
          pad-released (atom nil)]
      (midi/def-midi-pad! 36
        (fn [msg] (reset! pad-hit (:note msg)))
        (fn [msg] (reset! pad-released (:note msg))))

      ;; Note on triggers pad-hit
      (midi/handle-midi-msg {:cmd :note-on :data1 36 :data2 100})
      (is (= 36 @pad-hit))
      (is (nil? @pad-released))

      ;; Note off triggers pad-released
      (midi/handle-midi-msg {:cmd :note-off :data1 36 :data2 0})
      (is (= 36 @pad-released)))))

(deftest midi-pad-toggle-test
  (testing "binding MIDI pad toggle"
    (midi/reset-midi-state!)
    (let [state (atom :off)]
      (midi/def-midi-pad-toggle! 38
        (fn [_] (reset! state :on))
        (fn [_] (reset! state :off)))

      ;; First hit turns on
      (midi/handle-midi-msg {:cmd :note-on :data1 38 :data2 127})
      (is (= :on @state))

      ;; Second hit turns off
      (midi/handle-midi-msg {:cmd :note-on :data1 38 :data2 127})
      (is (= :off @state)))))

(deftest midi-debug-test
  (testing "toggling MIDI debug logging"
    (midi/reset-midi-state!)
    (is (false? (midi/midi-debug?)))
    (midi/midi-debug! true)
    (is (true? (midi/midi-debug?)))
    ;; Process CC message and verify formatting
    (let [log-event (midi/format-midi-debug-msg
                     {:cmd :control-change :data1 74 :data2 100 :channel 0})]
      (is (= {:midi-in :control-change :cc 74 :val 100 :channel 0}
             log-event)))
    ;; Process Note message and verify formatting
    (let [log-event (midi/format-midi-debug-msg
                     {:cmd :note-on :data1 36 :data2 120 :channel 0})]
      (is (= {:midi-in :note-on :note 36 :vel 120 :channel 0}
             log-event)))
    (midi/midi-debug! false)
    (is (false? (midi/midi-debug?)))))

(deftest midi-out-test
  (testing "midi-out state and sending"
    (midi/reset-midi-state!)
    (let [sent-events (atom [])
          fake-out {:name "Fake MIDI Out"
                    :receiver (reify Object)}
          mock-cc (fn [_out cc val & [chan]]
                    (swap! sent-events conj {:type :cc :cc cc :val val :chan (or chan 0)}))
          mock-note-on (fn [_out note vel & [chan]]
                         (swap! sent-events conj {:type :note-on :note note :vel vel :chan (or chan 0)}))
          mock-note-off (fn [_out note & [chan]]
                          (swap! sent-events conj {:type :note-off :note note :chan (or chan 0)}))]
      (with-redefs [overtone.midi/midi-control mock-cc
                    overtone.midi/midi-note-on mock-note-on
                    overtone.midi/midi-note-off mock-note-off]
        ;; Connect fake output
        (swap! midi/midi-state assoc-in [:connected-outputs "Fake MIDI Out"] fake-out)
        (swap! midi/midi-state assoc :default-output fake-out)

        ;; Test CC sending (knob value feedback)
        (midi/midi-send-cc! 1 64)
        (is (= [{:type :cc :cc 1 :val 64 :chan 0}] @sent-events))
        ;; Also updates local raw CC state
        (is (== 64.0 (get-in @midi/midi-state [:raw-cc 1])))

        ;; Test pad light / color (Note On)
        (midi/midi-set-pad-light! 36 127)
        (is (= [{:type :cc :cc 1 :val 64 :chan 0}
                {:type :note-on :note 36 :vel 127 :chan 0}]
               @sent-events))

        ;; Test pad light off (Note Off)
        (midi/midi-set-pad-light! 36 0)
        (is (= 3 (count @sent-events)))
        (is (= {:type :note-off :note 36 :chan 0} (last @sent-events)))

        ;; Test feedback runner
        (reset! sent-events [])
        (midi/test-midi-out! :ccs [1 2] :pads [36 37] :steps 2 :delay-ms 1)
        ;; Check that both CC sweeps and Pad flashes occurred
        (is (some #(= :cc (:type %)) @sent-events))
        (is (some #(= :note-on (:type %)) @sent-events)))))

  (testing "coordinate and note conversions"
    (is (= 0 (midi/coord->note 0 0)))
    (is (= 1 (midi/coord->note 0 1)))
    (is (= 16 (midi/coord->note 1 0)))
    (is (= 119 (midi/coord->note 7 7)))
    (is (= [0 0] (midi/note->coord 0)))
    (is (= [1 2] (midi/note->coord 18)))
    (is (= [7 7] (midi/note->coord 119))))

  (testing "color map"
    (is (= 0 (:black midi/colors)))
    (is (= 104 (:red midi/colors)))
    (is (= 88 (:green midi/colors)))
    (is (= 72 (:blue midi/colors))))

  (testing "light-on! and light-grid!"
    (midi/reset-midi-state!)
    (let [sent-events (atom [])
          fake-out {:name "Fake MIDI Out"
                    :receiver (reify Object)}
          mock-note-on (fn [_out note vel & [chan]]
                         (swap! sent-events conj {:type :note-on :note note :vel vel :chan (or chan 0)}))
          mock-note-off (fn [_out note & [chan]]
                          (swap! sent-events conj {:type :note-off :note note :chan (or chan 0)}))]
      (with-redefs [overtone.midi/midi-note-on mock-note-on
                    overtone.midi/midi-note-off mock-note-off]
        (swap! midi/midi-state assoc-in [:connected-outputs "Fake MIDI Out"] fake-out)
        (swap! midi/midi-state assoc :default-output fake-out)

        ;; Test light-on! by coordinate [row col]
        (midi/light-on! [0 1] :red)
        (is (= [{:type :note-off :note 1 :chan 0}
                {:type :note-on :note 1 :vel 104 :chan 0}]
               @sent-events))

        ;; Test light-on! by raw note
        (reset! sent-events [])
        (midi/light-on! 36 :green)
        (is (= [{:type :note-off :note 36 :chan 0}
                {:type :note-on :note 36 :vel 88 :chan 0}]
               @sent-events))

        ;; Test light-grid!
        (reset! sent-events [])
        (midi/light-grid! (fn [r c] :blue))
        ;; 8x8 = 64 pads, each gets note-off (reset) + note-on (blue 72)
        (is (= 128 (count @sent-events)))
        (is (every? #(or (= :note-off (:type %)) (= 72 (:vel %))) @sent-events))

        ;; Test random-lights returns a valid color keyword
        (let [col (midi/random-lights 0 0)]
          (is (contains? midi/colors col))
          (is (not= :black col)))))

  (testing "midi-set-pad-light! and light-grid! safely no-op when disconnected"
    (midi/reset-midi-state!)
    (is (nil? (midi/midi-set-pad-light! 36 :red)))
    (is (nil? (midi/light-grid! (fn [_ _] :red)))))

  (testing "rgb custom color builder"
    (is (= 0 (midi/rgb 0 0 0)))
    (is (= 104 (midi/rgb 1.0 0.0 0.0)))
    (is (= 104 (midi/rgb 255 0 0)))
    (is (= 88 (midi/rgb 0.0 1.0 0.0)))
    (is (= 88 (midi/rgb 0 255 0)))
    (is (= 72 (midi/rgb 0.0 0.0 1.0)))
    (is (= 24 (midi/rgb 1.0 1.0 0.0)))
    (is (= 40 (midi/rgb 0.0 1.0 1.0)))
    (is (= 56 (midi/rgb 1.0 0.0 1.0)))
    (is (= 8 (midi/rgb 1.0 1.0 1.0)))
    ;; Works directly with light-on!
    (let [sent-events (atom [])
          fake-out {:name "Fake MIDI Out"
                    :receiver (reify Object)}
          mock-note-on (fn [_out note vel & [chan]]
                         (swap! sent-events conj {:type :note-on
                                                  :note note
                                                  :vel vel
                                                  :chan (or chan 0)}))
          mock-note-off (fn [_out note & [chan]]
                          (swap! sent-events conj {:type :note-off
                                                   :note note
                                                   :chan (or chan 0)}))]
      (with-redefs [overtone.midi/midi-note-on mock-note-on
                    overtone.midi/midi-note-off mock-note-off]
        (swap! midi/midi-state assoc-in [:connected-outputs "Fake MIDI Out"]
               fake-out)
        (swap! midi/midi-state assoc :default-output fake-out)
        (midi/light-on! [2 3] (midi/rgb 1.0 0.0 0.0))
        (is (= [{:type :note-off :note 35 :chan 0}
                {:type :note-on :note 35 :vel 104 :chan 0}]
               @sent-events)))))

  (testing "light-grid pattern executes hooks on metronome schedule"
    (midi/reset-midi-state!)
    (let [grid-calls (atom [])
          scheduled-tasks (atom [])
          mock-apply-at (fn [target-metro-time f]
                          (swap! scheduled-tasks conj
                                 {:metro-time target-metro-time
                                  :fn f}))
          pat (p/light-grid [:red :blue :green :yellow])]
      (with-redefs [midi/light-grid! (fn [f]
                                       (swap! grid-calls conj (f 0 0)))
                    overtone.core/apply-at mock-apply-at
                    overtone.core/metro-bpm (constantly 120)
                    player/metro (fn [& [b]] (if b (+ 1000 b) 1000))]
        ;; Schedule cycle events at beat 0, cycle-dur 4.0
        (#'player/schedule-cycle-events :test-flashes 0 4.0 pat 0)
        ;; Should have scheduled 4 events at beat times 0, 1, 2, 3
        (is (= 4 (count @scheduled-tasks)))
        ;; Verify hooks did NOT execute eagerly during cycle scheduling
        (is (empty? @grid-calls))
        ;; Now execute each scheduled metronome callback in order
        (doseq [{:keys [fn]} @scheduled-tasks]
          (fn))
        ;; Grid colors should have executed sequentially in order
        (is (= [:red :blue :green :yellow] @grid-calls)))))))

