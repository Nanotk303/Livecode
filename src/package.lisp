(defpackage #:livecode
  (:use #:cl)
  (:shadow #:position)
  (:export
   ;; Main user API
   #:live
   #:stop-live
   #:panic
   #:live-status
   #:live-last-error
   #:live-inspect
   #:live-articulation-inspect
   #:live-sync-test
   #:use-livecode-safe-timing
   #:use-livecode-rock-solid-timing
   #:use-logging-midi
   #:use-direct-midi
   #:use-opusmodus-midi
   #:set-midi-sender
   #:set-midi-realtime-sender
   #:prepare-midi-realtime-sender
   #:prepare-midi-sysex-sender
   #:midi-clock-diagnostics
   #:list-midi-destinations
   #:test-midi-output
   #:test-midi-timestamped-output
   #:test-midi-mts-output
   #:test-midi-note-separation
   #:test-midi-clock-output
   #:reload-livecode-soundsets
   #:set-livecode-soundset-directory
   #:live-soundset-status
   #:live-soundset-program
   #:install-user-api
   ;; Useful extension/debugging API
   #:compile-scene
   #:compile-omn
   #:scene
   #:scene-events
   #:scene-length
   #:scene-tempo
   #:midi-event
   #:midi-event-beat
   #:midi-event-kind
   #:midi-event-channel
   #:midi-event-data1
   #:midi-event-data2
   #:midi-event-bytes
   #:midi-event-port
   #:*engine*
   #:*swap-quantum*
   #:*mts-enabled*
   #:*mts-device-id*
   #:*mts-tuning-program*
   #:*mts-lead-time-seconds*
   #:*sound-set-articulations-enabled*
   #:*keyswitch-lead-time-seconds*
   #:*keyswitch-duration-seconds*
   #:*keyswitch-velocity*
   #:*sound-set-message-lead-time-seconds*
   #:*send-redundant-articulation-messages*
   #:*sound-set-directory*
   #:*sound-set-auto-reload*
   #:*timestamp-midi-events*
   #:*midi-clock-start-delay-seconds*
   #:*midi-clock-event-offset-seconds*
   #:*event-wakeup-resolution-seconds*
   #:*retrigger-note-off-advance-beats*
   #:*event-schedule-ahead-seconds*
   #:*beats-per-bar*))

(defpackage #:livecode.tests
  (:use #:cl #:livecode)
  (:shadowing-import-from #:livecode #:position)
  (:export #:run-tests))
