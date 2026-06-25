(in-package #:livecode.tests)

(defvar *failures* nil)

(defmacro check (form)
  `(unless ,form
     (push ',form *failures*)))

(defparameter *controller-test-automation*
  '((0 1/128) (64 1/128) (127 1/128)))

(defun test-fallback-omn ()
  (multiple-value-bind (notes length)
      (compile-omn '(q c4 mf e d4 p -e q e4))
    (check (= length 3))
    (check (equal notes
                  '((0 1 60 76)
                    (1 1/2 62 48)
                    (2 1 64 48))))))

(defun test-nested-omn-phrases ()
  (multiple-value-bind (notes length)
      (compile-omn '((q c3 mf e d4 e e4 q g4)))
    (check (= length 3))
    (check (equal notes
                  '((0 1 48 76)
                    (1 1/2 62 76)
                    (3/2 1/2 64 76)
                    (2 1 67 76))))))

(defun test-chord-preservation ()
  (check (equal (livecode::flatten-pitch-events '(60 62 64 65))
                '(60 62 64 65)))
  ;; Opusmodus can wrap an entire generated monophonic phrase once more.
  ;; Its four durations disambiguate this from a four-note chord.
  (check (equal (livecode::flatten-pitch-events
                 '((60 62 64 65)) 4)
                '(60 62 64 65)))
  ;; With one corresponding duration, the same shape is genuinely a chord.
  (check (equal (livecode::flatten-pitch-events
                 '((60 62 64 65)) 1)
                '((60 62 64 65))))
  (check (equal (livecode::flatten-pitch-events '((60 64 67) 69))
                '((60 64 67) 69)))
  (check (equal (livecode::flatten-pitch-events
                 '(((62 65 69 76)) ((58 65 68 74))))
                '((62 65 69 76) (58 65 68 74))))
  (multiple-value-bind (notes length)
      (livecode::components-to-notes
       '(:length (4 4)
         :pitch ((62 65 69 76) (58 65 68 74))
         :velocity (76 76)))
    (check (= length 8))
    (check (equal notes
                  '((0 4 (62 65 69 76) 76)
                    (4 4 (58 65 68 74) 76))))))

(defun test-mts-frequency-bytes ()
  (check (equal (livecode::mts-frequency-bytes 60)
                '(60 0 0)))
  ;; Mirror the Nanotk MTS MIDI-CV module fixture:
  ;; 60 + #x20/#x80 = 60.25 semitones.
  (check (equal (livecode::mts-single-note-tuning-bytes 60 60.25d0)
                '(#xF0 #x7F #x7F #x08 #x02 0 1 60 60 #x20 0 #xF7)))
  (check (equal (livecode::mts-frequency-bytes 60.5d0)
                '(60 64 0)))
  (check (equal (livecode::mts-single-note-tuning-bytes 60 60.5d0)
                '(#xF0 #x7F #x7F #x08 #x02 0 1 60 60 64 0 #xF7))))

(defun test-microtonal-pitch-emits-mts-event ()
  (let ((event (livecode::maybe-mts-event 0 60.5d0 "Bus 1" 1 'lead)))
    (check event)
    (check (eq (midi-event-kind event) :mts))
    (check (= (midi-event-data1 event) 60))
    (check (equal (midi-event-bytes event)
                  '(#xF0 #x7F #x7F #x08 #x02 0 1 60 60 64 0 #xF7)))))

(defun test-opusmodus-flat-pitch-names ()
  (check (= (livecode::parse-pitch-symbol 'bb7) 106))
  (check (= (livecode::parse-pitch-symbol 'eb1) 27))
  (check (= (livecode::parse-pitch-symbol 'as0) 22)))

(defun test-sound-set-keyswitch-articulation ()
  (let ((livecode::*sound-set-program-resolver*
          (lambda (sound program)
            (when (and (eq sound 'fake-vsl)
                       (eq program 'leg))
              '(:key c1 :key bb7 :key eb1)))))
    (let* ((scene
             (compile-scene
              '((lead :omn (q c4)
                      :port "Bus 1"
                      :channel 1
                      :sound 'fake-vsl
                      :program '(leg)))
              :tempo 60))
           (events (scene-events scene))
           (keyswitch-ons
             (remove-if-not (lambda (event)
                              (eq (midi-event-kind event) :keyswitch-on))
                            events))
           (keyswitch-offs
             (remove-if-not (lambda (event)
                              (eq (midi-event-kind event) :keyswitch-off))
                            events)))
      (check (= (length keyswitch-ons) 3))
      (check (= (length keyswitch-offs) 3))
      (check (equal (mapcar #'midi-event-data1 keyswitch-ons)
                    '(24 106 27)))
      (check (= (count :note-on events :key #'midi-event-kind) 1)))))

(defun test-sound-set-registry-articulation ()
  (let ((livecode::*sound-set-auto-reload* nil)
        (livecode::*sound-set-program-resolver* nil))
    (clrhash livecode::*sound-set-registry*)
    (livecode::register-sound-set-form
     '(def-sound-set fake-vsl
       :supports-mts t
       :programs (:group omn
                  def (:key cs1 :key c2)
                  leg (:key d1 :key c2)))
     #P"test-soundset.lisp")
    (check (equal (live-soundset-program 'fake-vsl 'leg)
                  '(:key d1 :key c2)))
    (let* ((scene
             (compile-scene
              '((lead :omn (q c4)
                      :port "Bus 1"
                      :channel 1
                      :sound 'fake-vsl
                      :program '(leg)))
              :tempo 60))
           (keyswitch-ons
             (remove-if-not (lambda (event)
                              (eq (midi-event-kind event) :keyswitch-on))
                            (scene-events scene))))
      (check (equal (mapcar #'midi-event-data1 keyswitch-ons)
                    '(26 36))))))

(defun test-named-sound-set-controllers ()
  (let ((livecode::*sound-set-auto-reload* nil)
        (livecode::*sound-set-program-resolver* nil))
    (clrhash livecode::*sound-set-registry*)
    (livecode::register-sound-set-form
     '(def-sound-set fake-vsl
       :programs (:group omn def (:key cs1 :key c2))
       :controllers (:group default expression 11 velocity-xf 2))
     #P"test-soundset.lisp")
    (let* ((scene
             (compile-scene
              '((lead :omn (q c4)
                      :port "Bus 1"
                      :channel 1
                      :sound 'fake-vsl
                      :controllers (expression '(90)
                                    velocity-xf '((0 1/128)
                                                  (127 1/128)))))
              :tempo 60))
           (controllers
             (remove-if-not (lambda (event)
                              (eq (midi-event-kind event) :controller))
                            (scene-events scene))))
      (check (equal (sort (mapcar #'midi-event-data1 controllers) #'<)
                    '(2 2 11))))))

(defun test-composite-sound-set-articulation-events ()
  (let ((livecode::*sound-set-auto-reload* nil)
        (livecode::*sound-set-program-resolver* nil))
    (clrhash livecode::*sound-set-registry*)
    (livecode::register-sound-set-form
     '(def-sound-set fake-vsl
       :programs (:group omn
                  combo (expression 64 :key d1 8)
                  banked (3 12))
       :controllers (:group default expression 11))
     #P"test-soundset.lisp")
    (let* ((scene
             (compile-scene
              '((lead :omn (q c4)
                      :port "Bus 1"
                      :channel 1
                      :sound 'fake-vsl
                      :program '(combo)))
              :tempo 60))
           (events (scene-events scene)))
      (check (= (count :articulation-controller events
                       :key #'midi-event-kind)
                1))
      (check (= (count :keyswitch-on events :key #'midi-event-kind) 1))
      (check (= (count :articulation-program events
                       :key #'midi-event-kind)
                1))
      (check (= (midi-event-data1
                 (find :articulation-controller events
                       :key #'midi-event-kind))
                11)))
    (let* ((scene
             (compile-scene
              '((lead :omn (q c4)
                      :port "Bus 1"
                      :channel 1
                      :sound 'fake-vsl
                      :program '(banked)))
              :tempo 60))
           (events (scene-events scene)))
      (check (= (count :bank-select events :key #'midi-event-kind) 1))
      (check (= (count :articulation-program events
                       :key #'midi-event-kind)
                1)))))

(defun test-articulation-nil-placeholders-stay-aligned ()
  (check (equal (livecode::flatten-articulation-events
                 '((nil leg nil def)) 4)
                '(nil leg nil def)))
  (check (equal (livecode::source-articulation-events
                 '((s d4 p< leg e4 < leg f4 < leg g4 < def
                    q a4 f marc
                    h b4 ff trem+ponte))
                 6)
                '(leg leg leg def marc trem+ponte)))
  (check (equal (livecode::merge-articulation-events
                 '(- - - def marc trem+ponte)
                 '(leg leg leg def marc trem+ponte))
                '(leg leg leg def marc trem+ponte)))
  (multiple-value-bind (notes length)
      (livecode::components-to-notes
       '(:length (1 1 1 1)
         :pitch (60 62 64 65)
         :velocity (76 76 76 76)
         :articulation (nil leg nil def))
       :include-articulation t)
    (check (= length 4))
    (check (equal (mapcar #'fifth notes)
                  '(nil leg nil def)))))

(defun test-keyswitch-scheduling-surrounds-note-on ()
  (let ((livecode::*sound-set-program-resolver*
          (lambda (sound program)
            (when (and (eq sound 'fake-vsl)
                       (eq program 'leg))
              '(:key d1 :key c2)))))
    (let* ((scene
             (compile-scene
              '((lead :omn (q c4)
                      :port "Bus 1"
                      :channel 1
                      :sound 'fake-vsl
                      :program '(leg)))
              :tempo 60))
           (events (scene-events scene))
           (spb (livecode::seconds-per-beat scene))
           (deadlined
             (mapcar (lambda (event)
                       (list (midi-event-kind event)
                             (livecode::event-deadline scene event 100d0 spb)
                             (midi-event-data1 event)))
                     events))
           (first-keyswitch-on (find :keyswitch-on deadlined :key #'first))
           (note-on (find :note-on deadlined :key #'first))
           (first-keyswitch-off (find :keyswitch-off deadlined :key #'first)))
      (check first-keyswitch-on)
      (check note-on)
      (check first-keyswitch-off)
      (check (< (second first-keyswitch-on)
                (second note-on)
                (second first-keyswitch-off))))))

(defun test-idiomatic-omn-articulations ()
  (let ((livecode::*sound-set-program-resolver*
          (lambda (sound program)
            (when (eq sound 'fake-vsl)
              (case program
                (leg '(:key d1 :key c2))
                (def '(:key cs1 :key c2))
                (marc '(:key ds1 :key c2))
                ;; The test material says TREM+PONTE while the supplied VSL
                ;; Studio string soundset spells the same compound PONTE+TREM.
                (ponte+trem '(:key g1 :key ds2)))))))
    (let* ((scene
             (compile-scene
              '((vln :omn ((s d4 p< leg e4 < leg f4 < leg g4 < def
                              q a4 f marc
                              h b4 ff trem+ponte))
                      :port "Bus 1"
                      :channel 1
                      :sound 'fake-vsl
                      :program '(def)))
              :tempo 60))
           (events (scene-events scene))
           (notes (remove-if-not (lambda (event)
                                   (eq (midi-event-kind event) :note-on))
                                 events))
           (keyswitch-ons
             (remove-if-not (lambda (event)
                              (eq (midi-event-kind event) :keyswitch-on))
                            events)))
      (check (= (length notes) 6))
      (check (= (scene-length scene) 4))
      (check (equal (mapcar #'midi-event-data1 notes)
                    '(62 64 65 67 69 71)))
      ;; LEG is repeated on the first three sixteenth notes, but sound-set
      ;; messages are sent only when the effective articulation changes.
      (check (= (length keyswitch-ons) 8))
      (check (some (lambda (event)
                     (= (midi-event-data1 event) 31))
                   keyswitch-ons)))))

(defun test-redundant-articulations-can-be-reasserted ()
  (let ((livecode::*send-redundant-articulation-messages* t)
        (livecode::*sound-set-program-resolver*
          (lambda (sound program)
            (when (and (eq sound 'fake-vsl)
                       (eq program 'leg))
              '(:key d1 :key c2)))))
    (let* ((scene
             (compile-scene
              '((lead :omn (s c4 leg d4 leg e4 leg)
                      :port "Bus 1"
                      :channel 1
                      :sound 'fake-vsl
                      :program '(leg)))
              :tempo 60))
           (keyswitch-ons
             (remove-if-not (lambda (event)
                              (eq (midi-event-kind event) :keyswitch-on))
                            (scene-events scene))))
      (check (= (length keyswitch-ons) 6)))))

(defun test-scene ()
  (let* ((scene (compile-scene
                 '((lead :omn (q c4 mf q d4)
                         :port "Bus 1" :channel 2 :program 8
                         :controllers (1 ((127)) 11 ((55)))))
                 :tempo 65))
         (events (scene-events scene)))
    (check (= (scene-length scene) 2))
    (check (= (scene-tempo scene) 65))
    (check (= (count :note-on events :key #'midi-event-kind) 2))
    (check (= (count :note-off events :key #'midi-event-kind) 2))
    (check (= (count :controller events :key #'midi-event-kind) 2))
    (check (= (count :program events :key #'midi-event-kind) 1))
    (check (every (lambda (event)
                    (and (string= (midi-event-port event) "Bus 1")
                         (= (midi-event-channel event) 2)))
                  events))))

(defun test-controller-automation ()
  (let* ((scene
           (compile-scene
            '((lead :omn (q c4)
                    :port "Bus 1"
                    :channel 1
                    :controllers (1 '(127)
                                  11 '((0 1/128)
                                       (64 1/128)
                                       (127 1/128)))))
            :tempo 60))
         (variable-scene
           (compile-scene
            '((lead :omn (q c4)
                    :port "Bus 1"
                    :channel 1
                    :controllers (11 *controller-test-automation*)))
            :tempo 60))
         (controllers
           (remove-if-not (lambda (event)
                            (eq (midi-event-kind event) :controller))
                          (scene-events scene)))
         (cc11
           (remove-if-not (lambda (event)
                            (= (midi-event-data1 event) 11))
                          controllers)))
    (check (= (count 1 controllers :key #'midi-event-data1) 1))
    (check (= (count 11 controllers :key #'midi-event-data1) 3))
    (check (equal (mapcar #'midi-event-data2 cc11)
                  '(0 64 127)))
    (check (equal (mapcar #'midi-event-beat cc11)
                  '(0 1/32 1/16)))
    (check (= (count :controller (scene-events variable-scene)
                     :key #'midi-event-kind)
              3))))

(defun test-tempo-math ()
  (let ((scene-60
          (compile-scene
           '((lead :omn (q c4 q d4 q e4 q f4)
                   :port "Bus 1" :channel 1))
           :tempo 60))
        (scene-120
          (compile-scene
           '((lead :omn (q c4 q d4 q e4 q f4)
                   :port "Bus 1" :channel 1))
           :tempo 120)))
    ;; Four quarter notes last four seconds at 60 BPM and two at 120 BPM.
    (check (= 1d0 (livecode::seconds-per-beat scene-60)))
    (check (= 0.5d0 (livecode::seconds-per-beat scene-120)))
    (check (= 4d0 (livecode::scene-duration-seconds scene-60)))
    (check (= 2d0 (livecode::scene-duration-seconds scene-120)))))

(defun test-opusmodus-length-units ()
  (check (= 1 (livecode::opusmodus-length-to-beats 1/4)))
  (check (= 2 (livecode::opusmodus-length-to-beats 1/2)))
  (check (= 1/2 (livecode::opusmodus-length-to-beats 1/8)))
  (check (= -1 (livecode::opusmodus-length-to-beats -1/4))))

(defun test-clock-grid-does-not-drift ()
  (let ((scene
          (compile-scene
           '((lead :omn (q c4 q d4 q e4 q f4)
                   :port "Bus 1" :channel 1))
           :tempo 60)))
    ;; A small 20 ms overrun keeps the scheduled four-second boundary.
    (check (= 104d0
              (livecode::next-cycle-origin 104d0 scene 104.02d0)))
    ;; A suspension longer than a whole loop rebases the clock.
    (check (= 109d0
              (livecode::next-cycle-origin 104d0 scene 109d0)))))

(defun test-sync-test-scene ()
  (let* ((scene (livecode::make-sync-test-scene
                 :note-port "Bus 1"
                 :midi-clock-port "Bus 2"
                 :tempo 75
                 :beats 4
                 :click-channel 1
                 :clock-channel 2
                 :hold-notes '(60 64)))
         (events (scene-events scene)))
    (check (= (scene-tempo scene) 75))
    (check (= (scene-length scene) 4))
    (check (string= (livecode::scene-midi-clock-port scene) "Bus 2"))
    (check (= (count 'livecode::sync-click events
                     :key #'livecode::midi-event-track)
              8))
    (check (= (count 'livecode::sync-clock-source events
                     :key #'livecode::midi-event-track)
              4))
    (check (= (count :note-on events :key #'midi-event-kind) 6))
    (check (= (count :note-off events :key #'midi-event-kind) 6))))

(defun test-swap-quantization ()
  (let* ((scene
           (compile-scene
            '((lead :omn (w c4) :port "Bus 1" :channel 1))
            :tempo 60))
         (engine
           (livecode::make-engine
            :lock (livecode::make-engine-lock)
            :current-scene scene
            :cycle-start-time 100d0
            :beats-per-bar 4)))
    (check (= 101d0
              (livecode::next-quantized-time engine :beat 100.2d0)))
    (check (= 104d0
              (livecode::next-quantized-time engine :bar 100.2d0)))
    (check (= 104d0
              (livecode::next-quantized-time engine :cycle 100.2d0)))
    (check (= 100.5d0
              (livecode::next-quantized-time engine 1/2 100.2d0)))
    (check (= 100.2d0
              (livecode::next-quantized-time engine :immediate 100.2d0)))
    (let ((livecode::*timestamp-midi-events* t)
          (livecode::*event-schedule-ahead-seconds* 0.35d0)
          (livecode::*timestamped-swap-safety-margin-seconds* 0.02d0))
      ;; A bar at 104.0 is already inside the queued CoreMIDI window when the
      ;; user re-evaluates at 103.8, so the new scene must wait for 108.0 to
      ;; avoid a double downbeat.
      (check (= 108d0
                (livecode::next-live-swap-time engine :bar 103.8d0)))
      (check (< (abs (- 104.17d0
                        (livecode::next-live-swap-time
                         engine :immediate 103.8d0)))
                0.000001d0)))))

(defun test-midi-clock-events ()
  (let* ((scene
           (compile-scene
            '((lead :omn (q c4) :port "Bus 1" :channel 1))
            :tempo 120 :midi-clock-port "Bus 2")))
    (check (notany (lambda (event)
                     (eq :clock (midi-event-kind event)))
                   (scene-events scene)))
    (check (string= "Bus 2" (livecode::scene-midi-clock-port scene)))
    (check (= (/ 1d0 30d0) (livecode::midi-clock-interval 75)))))

(defun test-realtime-midi-has-one-byte ()
  (let (received)
    (let ((livecode::*midi-realtime-sender*
            (lambda (port status)
              (push (list port status) received))))
      (livecode::send-midi-realtime "Bus 2" #xF8)
      (livecode::send-midi-realtime "Bus 2" #xFA)
      (livecode::send-midi-realtime "Bus 2" #xFC))
    (check (equal (nreverse received)
                  '(("Bus 2" 248) ("Bus 2" 250) ("Bus 2" 252))))))

(defun test-one-argument-midi-sender ()
  (let (received)
    (let ((livecode::*midi-sender*
            (lambda (port status data1 data2)
              (declare (ignore port))
              (push (cond (data2 (list status data1 data2))
                          (data1 (list status data1))
                          (t (list status)))
                    received))))
      (livecode::send-midi-bytes "Bus 1" #x90 60 100)
      (livecode::send-midi-bytes "Bus 1" #xC0 8 nil)
      (livecode::send-midi-bytes "Bus 1" #xF8))
    (check (equal (nreverse received)
                  '((144 60 100) (192 8) (248))))))

(defparameter *test-midi-port* :outside)

(defun test-midi-thread-bindings ()
  (let ((livecode::*opusmodus-midi-bindings*
          (list (cons '*test-midi-port* :captured))))
    (check (eq :captured
               (livecode::call-with-midi-bindings
                (lambda () *test-midi-port*))))
    (check (eq :outside *test-midi-port*))))

(defun test-midi-status-decomposition ()
  (check (= 9 (ldb (byte 4 4) #x90)))
  (check (= 0 (ldb (byte 4 0) #x90)))
  (check (= 11 (ldb (byte 4 4) #xB4)))
  (check (= 4 (ldb (byte 4 0) #xB4))))

(defun test-channel-three-and-simultaneous-grouping ()
  (let* ((scene
           (compile-scene
            '((lead :omn (q c4) :port "Bus 1" :channel 1)
              (kick :omn (q c2) :port "Bus 1" :channel 3))
            :tempo 60))
         (seconds-per-beat (livecode::seconds-per-beat scene)))
    (multiple-value-bind (group deadline rest)
        (livecode::next-event-group (scene-events scene) scene 100d0
                                    seconds-per-beat)
      (declare (ignore rest))
      (check (= deadline 100d0))
      (check (= (length group) 2))
      (check (equal (sort (mapcar #'midi-event-channel group) #'<)
                    '(1 3)))
      (check (= (livecode::event-status-byte
                 (find 3 group :key #'midi-event-channel))
                #x92)))))

(defun test-note-on-priority-before-controller ()
  (let* ((scene
           (compile-scene
            '((lead :omn (q c4)
                    :port "Bus 1"
                    :channel 1
                    :controllers (1 '(64)))
              (kick :omn (q c2)
                    :port "Bus 1"
                    :channel 3))
            :tempo 60))
         (seconds-per-beat (livecode::seconds-per-beat scene)))
    (multiple-value-bind (group deadline rest)
        (livecode::next-event-group (scene-events scene) scene 100d0
                                    seconds-per-beat)
      (declare (ignore deadline rest))
      (let ((kinds (mapcar #'midi-event-kind group)))
        (check (< (cl:position :note-on kinds)
                  (cl:position :controller kinds)))))))

(defun test-repeated-kick-note-off-before-retrigger ()
  (let* ((scene
           (compile-scene
            '((kick :omn (q c2 q c2 q c2 q c2)
                    :port "Bus 1" :channel 3))
            :tempo 120))
         (kick-events
           (remove-if-not (lambda (event)
                            (and (= (midi-event-channel event) 3)
                                 (= (midi-event-data1 event) 36)))
                          (scene-events scene)))
         (note-ons
           (remove-if-not (lambda (event)
                            (eq (midi-event-kind event) :note-on))
                          kick-events))
         (note-offs
           (remove-if-not (lambda (event)
                            (eq (midi-event-kind event) :note-off))
                          kick-events)))
    (check (equal (mapcar #'midi-event-beat note-ons)
                  '(0 1 2 3)))
    (check (equal (butlast (mapcar #'midi-event-beat note-offs))
                  (list (- 1 livecode::*retrigger-note-off-advance-beats*)
                        (- 2 livecode::*retrigger-note-off-advance-beats*)
                        (- 3 livecode::*retrigger-note-off-advance-beats*))))
    (check (= (midi-event-beat (car (last note-offs)))
              (- 4 livecode::*retrigger-note-off-advance-beats*)))))

(defun test-track-gate-beats ()
  (let* ((scene
           (compile-scene
            '((kick :omn (q c2 q c2 q c2 q c2)
                    :port "Bus 1" :channel 3 :gate 1/16))
            :tempo 120))
         (kick-events
           (remove-if-not (lambda (event)
                            (and (= (midi-event-channel event) 3)
                                 (= (midi-event-data1 event) 36)))
                          (scene-events scene)))
         (note-ons
           (remove-if-not (lambda (event)
                            (eq (midi-event-kind event) :note-on))
                          kick-events))
         (note-offs
           (remove-if-not (lambda (event)
                            (eq (midi-event-kind event) :note-off))
                          kick-events)))
    (check (equal (mapcar #'midi-event-beat note-ons)
                  '(0 1 2 3)))
    (check (equal (mapcar #'midi-event-beat note-offs)
                  '(1/16 17/16 33/16 49/16)))))

(defun test-retrigger-note-offs-are-not-pre-paired ()
  (let ((livecode::*retrigger-note-off-advance-beats* 0))
    (let* ((scene
             (compile-scene
              '((kick :omn (q c2 q c2 q c2 q c2)
                      :port "Bus 1" :channel 3))
              :tempo 120))
           (seconds-per-beat (livecode::seconds-per-beat scene))
           (events (scene-events scene)))
      ;; First group: beat-0 note-on.  Its note-off lands at beat 1, exactly
      ;; where the next same-note note-on happens.  That note-off must not be
      ;; pulled out and timestamped separately, otherwise CoreMIDI/receivers
      ;; may process same-timestamp note-on/note-off in the wrong order.
      (multiple-value-bind (group deadline rest)
          (livecode::next-event-group events scene 100d0 seconds-per-beat)
        (declare (ignore deadline))
        (let* ((note-on (find :note-on group :key #'midi-event-kind))
               (note-off
                 (livecode::find-matching-note-off note-on rest)))
          (check note-off)
          (check (not (livecode::safe-paired-note-off-p
                       note-off rest scene))))))))

(defun test-timestamped-swap-blocks-old-boundary-events ()
  (let* ((scene
           (compile-scene
            '((kick :omn (q c2 q c2)
                    :port "Bus 1" :channel 3))
            :tempo 120))
         (engine
           (livecode::make-engine
            :lock (livecode::make-engine-lock)
            :current-scene scene
            :pending-scene scene
            :pending-start-time 101d0)))
    (check (not (livecode::event-at-or-after-pending-swap-p
                 engine 100.5d0)))
    (check (livecode::event-at-or-after-pending-swap-p
            engine 101d0))
    (check (livecode::event-at-or-after-pending-swap-p
            engine 101.5d0))))

(defun test-paired-note-off-does-not-cross-swap ()
  (let* ((scene
           (compile-scene
            '((pad :omn (w c4)
                   :port "Bus 1" :channel 1))
            :tempo 60))
         (seconds-per-beat (livecode::seconds-per-beat scene))
         (events (scene-events scene))
         (note-off (find :note-off events :key #'midi-event-kind)))
    (check (livecode::safe-paired-note-off-p
            note-off events scene 100d0 seconds-per-beat 105d0))
    (check (not (livecode::safe-paired-note-off-p
                 note-off events scene 100d0 seconds-per-beat 103d0)))))

(defun test-cycle-boundary-predispatches-next-initial-events ()
  (let ((livecode::*retrigger-note-off-advance-beats* 0)
        (captured nil))
    (let* ((scene
             (compile-scene
              '((kick :omn (q c2) :port "Bus 1" :channel 3))
              :tempo 120))
           (engine
             (livecode::make-engine
              :lock (livecode::make-engine-lock)
              :running-p t
              :current-scene scene))
           (livecode::*midi-sender*
             (lambda (port status data1 data2)
               (declare (ignore port))
               (push (list status data1 data2) captured))))
      (multiple-value-bind (result next-start skip-next-initial-p)
          (livecode::run-one-cycle engine scene
                                   (- (livecode::monotonic-seconds) 2d0)
                                   nil)
        (declare (ignore next-start))
        (check (eq result :complete))
        (check skip-next-initial-p)
        ;; First cycle note-on, boundary note-off, next-cycle beat-0 note-on.
        (check (equal (mapcar #'first (nreverse captured))
                      '(#x92 #x82 #x92)))))))

(defun test-link-scene-inspection ()
  (let ((summary
          (live-inspect
           '((lead :omn (q c4) :port "Bus 1" :channel 1))
           :tempo 120
           :link t
           :link-quantum 4
           :link-start-stop t
           :limit 0)))
    (check (eq (getf summary :link-enabled) t))
    (check (= (getf summary :link-quantum) 4))
    (check (eq (getf summary :link-start-stop) t))
    (check (= (getf summary :notes) 1))))

(defun test-link-cycle-origin-keeps-scheduled-grid ()
  (let ((scene (livecode::make-scene :length 4
                                     :tempo 120
                                     :link-enabled t)))
    ;; At a Link cycle boundary the scheduler may return a few milliseconds
    ;; late. That must not be interpreted as "wait until the next Link
    ;; boundary", otherwise a whole cycle can go silent before playback resumes.
    (check (= (livecode::next-cycle-origin 100d0 scene 100.01d0)
              100d0))))

(defun test-link-initial-start-uses-link-quantum ()
  (let ((scene (livecode::make-scene :length 32
                                     :tempo 120
                                     :link-enabled t
                                     :link-quantum 4))
        (original (symbol-function
                   'livecode::ableton-link-next-quantized-time))
        seen-quantum)
    (unwind-protect
         (progn
           (setf (symbol-function
                  'livecode::ableton-link-next-quantized-time)
                 (lambda (quantum &optional minimum-time)
                   (declare (ignore minimum-time))
                   (setf seen-quantum quantum)
                   123d0))
           (check (= (livecode::link-scene-cycle-start-time scene) 123d0))
           (check (= seen-quantum 4)))
      (setf (symbol-function
             'livecode::ableton-link-next-quantized-time)
            original))))

(defun run-tests ()
  (setf *failures* nil)
  (test-fallback-omn)
  (test-nested-omn-phrases)
  (test-chord-preservation)
  (test-mts-frequency-bytes)
  (test-microtonal-pitch-emits-mts-event)
  (test-opusmodus-flat-pitch-names)
  (test-sound-set-keyswitch-articulation)
  (test-sound-set-registry-articulation)
  (test-named-sound-set-controllers)
  (test-composite-sound-set-articulation-events)
  (test-articulation-nil-placeholders-stay-aligned)
  (test-keyswitch-scheduling-surrounds-note-on)
  (test-idiomatic-omn-articulations)
  (test-redundant-articulations-can-be-reasserted)
  (test-scene)
  (test-controller-automation)
  (test-tempo-math)
  (test-opusmodus-length-units)
  (test-clock-grid-does-not-drift)
  (test-sync-test-scene)
  (test-swap-quantization)
  (test-midi-clock-events)
  (test-realtime-midi-has-one-byte)
  (test-one-argument-midi-sender)
  (test-midi-thread-bindings)
  (test-midi-status-decomposition)
  (test-channel-three-and-simultaneous-grouping)
  (test-note-on-priority-before-controller)
  (test-repeated-kick-note-off-before-retrigger)
  (test-track-gate-beats)
  (test-retrigger-note-offs-are-not-pre-paired)
  (test-timestamped-swap-blocks-old-boundary-events)
  (test-paired-note-off-does-not-cross-swap)
  (test-cycle-boundary-predispatches-next-initial-events)
  (test-link-scene-inspection)
  (test-link-cycle-origin-keeps-scheduled-grid)
  (test-link-initial-start-uses-link-quantum)
  ;; The scheduler itself is exercised separately because timing tests are
  ;; intentionally not part of the deterministic core suite.
  (if *failures*
      (error "~D Livecode test(s) failed:~%~{  ~S~%~}"
             (length *failures*) (nreverse *failures*))
      (progn
        (format t "~&All Livecode tests passed.~%")
        t)))
