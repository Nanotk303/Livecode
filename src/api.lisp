(in-package #:livecode)

(defun live (tracks &key (tempo 120)
                         (quantize *swap-quantum*)
                         (beats-per-bar *beats-per-bar*)
                         midi-clock-port
                         link
                         (link-quantum *ableton-link-default-quantum*)
                         (link-start-stop
                          *ableton-link-start-stop-sync*))
  "Start Livecode or queue a replacement scene.

TRACKS is a quoted list of instrument forms:
  ((name :omn source :port \"Bus 1\" :channel 1 :program 8 ...))

If the clock already runs, the new scene becomes active on the next loop
boundary. The clock thread and its time origin are not restarted.

When LINK is true, Livecode follows the Ableton Link timeline through the
optional native bridge. LINK-QUANTUM is the shared Link phase quantum."
  (submit-scene (compile-scene tracks :tempo tempo
                               :midi-clock-port midi-clock-port
                               :link link
                               :link-quantum link-quantum
                               :link-start-stop link-start-stop)
                :quantize quantize
                :beats-per-bar beats-per-bar))

(defun event-summary (event)
  (list :beat (midi-event-beat event)
        :kind (midi-event-kind event)
        :channel (midi-event-channel event)
        :data1 (midi-event-data1 event)
        :data2 (midi-event-data2 event)
        :bytes (midi-event-bytes event)
        :port (midi-event-port event)
        :track (midi-event-track event)))

(defun live-inspect (tracks &key (tempo 120) midi-clock-port
                                 link
                                 (link-quantum *ableton-link-default-quantum*)
                                 (link-start-stop
                                  *ableton-link-start-stop-sync*)
                                 (limit 24))
  "Compile TRACKS without starting playback and return a compact summary."
  (let* ((scene (compile-scene tracks :tempo tempo
                               :midi-clock-port midi-clock-port
                               :link link
                               :link-quantum link-quantum
                               :link-start-stop link-start-stop))
         (events (scene-events scene)))
    (list :tempo (scene-tempo scene)
          :loop-beats (scene-length scene)
          :midi-clock-port (scene-midi-clock-port scene)
          :link-enabled (scene-link-enabled scene)
          :link-quantum (and (scene-link-enabled scene)
                             (scene-link-quantum scene))
          :link-start-stop (and (scene-link-enabled scene)
                                (scene-link-start-stop scene))
          :events (length events)
          :notes (count :note-on events :key #'midi-event-kind)
          :mts (count :mts events :key #'midi-event-kind)
          :keyswitches (count :keyswitch-on events :key #'midi-event-kind)
          :controllers (count :controller events :key #'midi-event-kind)
          :articulation-controllers
          (count :articulation-controller events :key #'midi-event-kind)
          :programs (count :program events :key #'midi-event-kind)
          :articulation-programs
          (count :articulation-program events :key #'midi-event-kind)
          :bank-selects
          (count :bank-select events :key #'midi-event-kind)
          :first-events (mapcar #'event-summary
                                (subseq events 0
                                        (min limit (length events)))))))

(defun live-articulation-inspect (tracks)
  "Inspect OMN articulations and their resolved sound-set keyswitches."
  (mapcar #'inspect-track-articulations
          (resolve-source tracks)))

(defun make-sync-test-scene (&key (note-port "Bus 1")
                                  (midi-clock-port "Bus 2")
                                  (tempo 75)
                                  (beats 16)
                                  (click-channel 1)
                                  (clock-channel 2)
                                  (click-note 84)
                                  (hold-notes '(60 64 67 72))
                                  (velocity 100)
                                  (click-duration 1/8))
  "Build a deterministic scene for measuring Livecode vs clock-driven timing.

The click channel emits short direct MIDI notes on every beat. The clock
channel holds HOLD-NOTES for the full loop so an external clock-synced
arp/gate can produce its own clicks from Livecode's MIDI Clock."
  (unless (and (integerp beats) (plusp beats))
    (error "BEATS must be a positive integer, got ~S." beats))
  (check-type click-channel (integer 1 16))
  (check-type clock-channel (integer 1 16))
  (let ((events nil))
    (loop for beat below beats
          do (push (make-midi-event :beat beat
                                    :kind :note-on
                                    :channel click-channel
                                    :data1 (clamp-midi click-note)
                                    :data2 (clamp-midi velocity)
                                    :port note-port
                                    :track 'sync-click)
                   events)
             (push (make-midi-event :beat (+ beat click-duration)
                                    :kind :note-off
                                    :channel click-channel
                                    :data1 (clamp-midi click-note)
                                    :data2 0
                                    :port note-port
                                    :track 'sync-click)
                   events))
    (dolist (note (if (listp hold-notes) hold-notes (list hold-notes)))
      (push (make-midi-event :beat 0
                             :kind :note-on
                             :channel clock-channel
                             :data1 (clamp-midi note)
                             :data2 (clamp-midi velocity)
                             :port note-port
                             :track 'sync-clock-source)
            events)
      (push (make-midi-event :beat beats
                             :kind :note-off
                             :channel clock-channel
                             :data1 (clamp-midi note)
                             :data2 0
                             :port note-port
                             :track 'sync-clock-source)
            events))
    (make-scene :events (stable-sort events #'< :key #'midi-event-beat)
                :length beats
                :tempo tempo
                :midi-clock-port midi-clock-port
                :source :sync-test)))

(defun live-sync-test (&key (note-port "Bus 1")
                            (midi-clock-port "Bus 2")
                            (tempo 75)
                            (beats 16)
                            (click-channel 1)
                            (clock-channel 2)
                            (click-note 84)
                            (hold-notes '(60 64 67 72))
                            (velocity 100)
                            (quantize :immediate)
                            (beats-per-bar *beats-per-bar*))
  "Start a deterministic synchronization calibration loop.

Route CLICK-CHANNEL to a very short percussive sound. Route CLOCK-CHANNEL
through the VEPro/plugin path that is driven by the MIDI Clock. Record both
audio outputs and measure the sample offset between transients."
  (submit-scene (make-sync-test-scene
                 :note-port note-port
                 :midi-clock-port midi-clock-port
                 :tempo tempo
                 :beats beats
                 :click-channel click-channel
                 :clock-channel clock-channel
                 :click-note click-note
                 :hold-notes hold-notes
                 :velocity velocity)
                :quantize quantize
                :beats-per-bar beats-per-bar))

(defun test-midi-mts-output (&key (port "Bus 1") (channel 1)
                                  (pitch 60.5d0) (velocity 100)
                                  (start-delay 0.15d0)
                                  (duration 0.50d0))
  "Play one microtonal test note using MTS plus timestamped note on/off."
  (check-type channel (integer 1 16))
  (unless (realp pitch)
    (error "PITCH must be a real MIDI pitch number, got ~S." pitch))
  (prepare-midi-timestamped-sender)
  (prepare-midi-sysex-sender)
  (let* ((key (midi-key-for-pitch pitch))
         (now (core-midi-host-time))
         (start-time
           (+ now (core-midi-seconds-to-host-ticks start-delay)))
         (mts-time
           (+ now
              (core-midi-seconds-to-host-ticks
               (max 0.001d0
                    (- (coerce start-delay 'double-float)
                       (coerce *mts-lead-time-seconds* 'double-float))))))
         (stop-time
           (+ start-time (core-midi-seconds-to-host-ticks duration))))
    (when (microtonal-pitch-p pitch)
      (send-midi-sysex-at-host-time
       port (mts-single-note-tuning-bytes key pitch) mts-time))
    (send-midi-bytes-at-host-time port (+ #x90 (1- channel))
                                  key velocity start-time)
    (send-midi-bytes-at-host-time port (+ #x80 (1- channel))
                                  key 0 stop-time)
    (sleep (+ (coerce start-delay 'double-float)
              (coerce duration 'double-float)
              0.10d0)))
  :ok)

(defun use-livecode-safe-timing ()
  "Use the proven direct MIDI path with the tight Lisp scheduler.

This is the safest mode when testing a new Opusmodus/LispWorks setup."
  (setf *timestamp-midi-events* nil
        *event-wakeup-resolution-seconds* 0.001d0
        *midi-clock-event-offset-seconds* 0d0)
  (list :timestamp-midi-events *timestamp-midi-events*
        :event-wakeup-resolution *event-wakeup-resolution-seconds*
        :midi-clock-event-offset *midi-clock-event-offset-seconds*))

(defun use-livecode-rock-solid-timing (&key (ahead 0.25d0))
  "Use timestamped CoreMIDI scheduling for ordinary MIDI events.

This adds a small livecoding latency window, but MIDI note/controller/program
placement is handed to CoreMIDI instead of depending on the exact wake-up time
of the Lisp scheduler thread."
  (unless (and (realp ahead) (plusp ahead))
    (error "AHEAD must be a positive number of seconds, got ~S." ahead))
  (prepare-midi-timestamped-sender)
  (prepare-midi-sysex-sender)
  (unless (midi-event-scheduling-supported-p)
    (error "CoreMIDI timestamped event scheduling is not available."))
  (setf *timestamp-midi-events* t
        *event-schedule-ahead-seconds* (coerce ahead 'double-float)
        *event-wakeup-resolution-seconds* 0.001d0
        *midi-clock-event-offset-seconds* 0d0)
  (list :timestamp-midi-events *timestamp-midi-events*
        :midi-events-scheduled (midi-event-scheduling-supported-p)
        :event-schedule-ahead *event-schedule-ahead-seconds*
        :event-wakeup-resolution *event-wakeup-resolution-seconds*
        :midi-clock-event-offset *midi-clock-event-offset-seconds*))

(defun install-user-api (&optional extra-package)
  "Install the short Livecode commands in the usual Opusmodus packages.

EXTRA-PACKAGE can be a package object or package designator. This is useful
when a workspace evaluates forms in a custom package."
  (let ((packages
          (remove-duplicates
           (remove nil
                   (append
                    (list (find-package :cl-user)
                          (find-package "OPUSMODUS")
                          (find-package "OM"))
                    (when extra-package
                      (list (find-package extra-package)))))
           :test #'eq)))
    (dolist (package packages)
      (handler-case
          (setf (fdefinition (intern "LIVE" package)) #'live
                (fdefinition (intern "STOP-LIVE" package)) #'stop-live
                (fdefinition (intern "LIVE-STATUS" package)) #'live-status
                (fdefinition (intern "LIVE-INSPECT" package)) #'live-inspect
                (fdefinition (intern "LIVE-ARTICULATION-INSPECT" package))
                #'live-articulation-inspect
                (fdefinition (intern "LIVE-SYNC-TEST" package))
                #'live-sync-test
                (fdefinition (intern "USE-LIVECODE-SAFE-TIMING" package))
                #'use-livecode-safe-timing
                (fdefinition (intern "USE-LIVECODE-ROCK-SOLID-TIMING" package))
                #'use-livecode-rock-solid-timing
                (fdefinition (intern "LOAD-ABLETON-LINK" package))
                #'load-ableton-link
                (fdefinition (intern "USE-ABLETON-LINK" package))
                #'use-ableton-link
                (fdefinition (intern "STOP-ABLETON-LINK" package))
                #'stop-ableton-link
                (fdefinition (intern "ABLETON-LINK-STATUS" package))
                #'ableton-link-status
                (fdefinition (intern "ABLETON-LINK-START" package))
                #'ableton-link-start
                (fdefinition (intern "ABLETON-LINK-STOP" package))
                #'ableton-link-stop
                (fdefinition (intern "TEST-MIDI-TIMESTAMPED-OUTPUT" package))
                #'test-midi-timestamped-output
                (fdefinition (intern "TEST-MIDI-MTS-OUTPUT" package))
                #'test-midi-mts-output
                (fdefinition (intern "RELOAD-LIVECODE-SOUNDSETS" package))
                #'reload-livecode-soundsets
                (fdefinition (intern "LIVE-SOUNDSET-STATUS" package))
                #'live-soundset-status
                (fdefinition (intern "LIVE-SOUNDSET-PROGRAM" package))
                #'live-soundset-program
                (fdefinition (intern "LIVE-LAST-ERROR" package))
                #'live-last-error
                (fdefinition (intern "LIVE-PANIC" package)) #'panic)
        (error (condition)
          (warn "Could not install the Livecode API in package ~A: ~A"
                (package-name package) condition))))
    (mapcar #'package-name packages)))

;; Make (live ...) available immediately after loading, without requiring a
;; package-qualified call.
(eval-when (:load-toplevel :execute)
  (install-user-api))
