(in-package #:livecode)

(defun monotonic-seconds ()
  (/ (coerce (get-internal-real-time) 'double-float)
     (coerce internal-time-units-per-second 'double-float)))

(defun sleep-until (deadline)
  (loop
    for remaining = (- deadline (monotonic-seconds))
    while (plusp remaining)
    do (sleep (min remaining 0.02d0))))

(defun precise-sleep-until (deadline)
  "Wait for DEADLINE without relying on the final millisecond of OS sleep.

The short final spin is intentional for MIDI Clock. It avoids several
milliseconds of wake-up jitter while consuming CPU only immediately before
each clock pulse."
  (loop
    for remaining = (- deadline (monotonic-seconds))
    while (plusp remaining)
    when (> remaining 0.0015d0)
      do (sleep (- remaining 0.001d0))))

#+lispworks
(progn
  (fli:define-c-struct mach-timebase-info
    (numer :uint32)
    (denom :uint32))

  (fli:define-foreign-function (%mach-absolute-time "mach_absolute_time")
      ()
    ;; LispWorks/Opusmodus accepts this symbolic type here; if the host-time
    ;; call itself fails, MIDI-CLOCK-DIAGNOSTICS reports the exact condition.
    :result-type :uint64)

  (fli:define-foreign-function (%mach-timebase-info "mach_timebase_info")
      ((info (:pointer (:struct mach-timebase-info))))
    :result-type :int)

  (defvar *core-midi-host-timebase* nil)

  (defun core-midi-host-time ()
    "Return the macOS host clock used by CoreMIDI MIDITimeStamp."
    (%mach-absolute-time))

  (defun core-midi-host-timebase ()
    "Return two values: nanoseconds-per-host-tick numerator and denominator."
    (or *core-midi-host-timebase*
        (setf *core-midi-host-timebase*
              (fli:with-dynamic-foreign-objects
                  ((info (:struct mach-timebase-info)))
                (let ((result (%mach-timebase-info info)))
                  (unless (zerop result)
                    (error "mach_timebase_info failed with code ~A."
                           result))
                  (cons (fli:foreign-slot-value
                         info 'numer
                         :object-type '(:struct mach-timebase-info))
                        (fli:foreign-slot-value
                         info 'denom
                         :object-type '(:struct mach-timebase-info))))))))

  (defun core-midi-seconds-to-host-ticks (seconds)
    (let ((timebase (core-midi-host-timebase)))
      (round (* (coerce seconds 'double-float)
                1000000000d0
                (cdr timebase))
             (car timebase)))))

#-lispworks
(progn
  (defun core-midi-host-time () nil)
  (defun core-midi-host-timebase () nil)
  (defun core-midi-seconds-to-host-ticks (seconds)
    (declare (ignore seconds))
    nil))

#+lispworks
(progn
  (defun make-engine-lock ()
    (mp:make-lock :name "Livecode engine lock"))

  (defmacro with-engine-lock ((engine) &body body)
    `(mp:with-lock ((engine-lock ,engine))
       ,@body))

  (defun spawn-engine-thread (function &optional (name "Livecode scheduler"))
    (mp:process-run-function name nil function))

  (defun thread-alive-p (thread)
    (and thread (mp:process-alive-p thread))))

#+sbcl
(progn
  (defun make-engine-lock ()
    (sb-thread:make-mutex :name "Livecode engine lock"))

  (defmacro with-engine-lock ((engine) &body body)
    `(sb-thread:with-mutex ((engine-lock ,engine))
       ,@body))

  (defun spawn-engine-thread (function &optional (name "Livecode scheduler"))
    (sb-thread:make-thread function :name name))

  (defun thread-alive-p (thread)
    (and thread (sb-thread:thread-alive-p thread))))

#-(or lispworks sbcl)
(progn
  (defun make-engine-lock () nil)

  (defmacro with-engine-lock ((engine) &body body)
    (declare (ignore engine))
    `(progn ,@body))

  (defun spawn-engine-thread (function &optional name)
    (declare (ignore name))
    (error "Livecode needs LispWorks or SBCL thread support, got ~A."
           (lisp-implementation-type)))

  (defun thread-alive-p (thread)
    (declare (ignore thread))
    nil))
