(in-package #:livecode)

(defstruct midi-event
  (beat 0 :type rational)
  (kind :note-on :type keyword)
  (channel 1 :type (integer 1 16))
  (data1 0 :type (integer 0 127))
  (data2 0 :type (integer 0 127))
  bytes
  port
  track)

(defstruct scene
  (events nil :type list)
  (length 4 :type rational)
  (tempo 120 :type real)
  midi-clock-port
  (link-enabled nil)
  (link-quantum 4 :type real)
  (link-start-stop nil)
  source)

(defstruct engine
  thread
  lock
  (running-p nil)
  current-scene
  pending-scene
  pending-start-time
  (pending-quantization :bar)
  (beats-per-bar 4 :type rational)
  (cycle-number 0 :type integer)
  (cycle-start-time 0d0 :type double-float)
  (skip-initial-events-p nil)
  midi-clock-thread
  (midi-clock-running-p nil)
  midi-clock-port
  (midi-clock-tempo 120 :type real)
  (midi-clock-start-time 0d0 :type double-float)
  (last-error nil))

(defvar *engine* nil)
(defvar *livecode-engines* nil
  "All engines created in this Lisp image, retained across source reloads.")

(defparameter *swap-quantum* :bar
  "Default replacement boundary: :IMMEDIATE, :BEAT, :BAR, :CYCLE or beats.")

(defparameter *beats-per-bar* 4
  "Default bar length in quarter-note beats.")
