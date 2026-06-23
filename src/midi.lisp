(in-package #:livecode)

(defparameter *midi-sender* nil)
(defparameter *active-notes* (make-hash-table :test #'equal))
(defparameter *opusmodus-midi-function-name* nil)
(defparameter *opusmodus-midi-bindings* nil)
(defparameter *direct-midi-destinations* (make-hash-table :test #'equalp))
(defparameter *midi-realtime-sender* nil)
(defparameter *midi-realtime-sender-kind* nil)
(defparameter *midi-timestamped-sender* nil)
(defparameter *midi-timestamped-sender-kind* nil)
(defparameter *midi-sysex-sender* nil)
(defparameter *midi-sysex-sender-kind* nil)

(defun clamp-midi (value)
  (max 0 (min 127 (round value))))

(defun clamp-midi-status (value)
  (max 0 (min 255 (round value))))

(defun logging-midi-sender (port status data1 data2)
  (format t "~&[Livecode MIDI] ~A  ~2,'0X~@[ ~3D~]~@[ ~3D~]~%"
          port status data1 data2))

(defun use-logging-midi ()
  (setf *midi-sender* #'logging-midi-sender)
  :logging)

(defun set-midi-sender (function)
  "Install FUNCTION of four arguments: PORT STATUS DATA1 DATA2."
  (check-type function function)
  (setf *midi-sender* function)
  function)

(defun set-midi-realtime-sender (function)
  "Install FUNCTION of two arguments: PORT and one-byte STATUS."
  (check-type function function)
  (setf *midi-realtime-sender* function
        *midi-realtime-sender-kind* :custom)
  function)

(defun call-candidate (function arguments)
  (handler-case
      (progn (apply function arguments) t)
    (error () nil)))

(defun find-bound-symbol (name)
  (dolist (package-name '("OPUSMODUS" "SEQUENCER" "CL-MIDI"))
    (let ((package (find-package package-name)))
      (when package
        (multiple-value-bind (symbol status) (find-symbol name package)
          (when (and status (boundp symbol))
            (return-from find-bound-symbol symbol))))))
  (dolist (package (list-all-packages))
    (multiple-value-bind (symbol status) (find-symbol name package)
      (when (and status (boundp symbol))
        (return-from find-bound-symbol symbol)))))

(defun function-in-package (name package-name)
  (let ((package (find-package package-name)))
    (when package
      (multiple-value-bind (symbol status) (find-symbol name package)
        (when (and status (fboundp symbol))
          (values (symbol-function symbol) symbol))))))

(defun require-function-in-package (name package-name)
  (multiple-value-bind (function symbol)
      (function-in-package name package-name)
    (unless function
      (error "Required function ~A::~A was not found." package-name name))
    (values function symbol)))

(defun require-opusmodus-function (name)
  (multiple-value-bind (function symbol) (opusmodus-function name)
    (unless function
      (error "Required Opusmodus function ~A was not found." name))
    (values function symbol)))

(defun capture-opusmodus-midi-bindings ()
  "Capture MIDI specials whose dynamic values are not inherited reliably by
the LispWorks clock process."
  (loop for name in '("*MIDI-OUTPUT-PORT*" "*MIDI-OUTPUT-DIR*")
        for symbol = (find-bound-symbol name)
        when symbol
          collect (cons symbol (symbol-value symbol))))

(defun call-with-midi-bindings (function)
  (let ((symbols (mapcar #'car *opusmodus-midi-bindings*))
        (values (mapcar #'cdr *opusmodus-midi-bindings*)))
    (progv symbols values
      (funcall function))))

(defun captured-midi-output-port ()
  (loop for (symbol . value) in *opusmodus-midi-bindings*
        when (string= (symbol-name symbol) "*MIDI-OUTPUT-PORT*")
          return value))

(defun make-opusmodus-sender ()
  "Build an adapter around Opusmodus' byte-oriented MIDI helper.

MIDI-SEND itself is deliberately not used: in current Opusmodus releases it
is a low-level CoreMIDI function taking (PORT DEST PKTLIST)."
  (multiple-value-bind (function symbol)
      (opusmodus-function "%SEND-MIDI-BYTES")
    (unless function
      (error "Opusmodus function %SEND-MIDI-BYTES was not found."))
    (setf *opusmodus-midi-function-name* symbol
          *opusmodus-midi-bindings* (capture-opusmodus-midi-bindings))
    (lambda (port status data1 data2)
      (let ((bytes (cond (data2 (list status data1 data2))
                         (data1 (list status data1))
                         (t (list status)))))
        ;; Opusmodus 3 exposes %SEND-MIDI-BYTES as (BYTES). It sends through
        ;; the MIDI destination currently selected/configured by Opusmodus.
        (call-with-midi-bindings
         (lambda ()
           (or (call-candidate function (list bytes))
               (call-candidate function (list bytes port))
               (call-candidate function (list port bytes))
               (call-candidate function bytes)
               (and data2
                    (call-candidate function
                                    (list status data1 data2 port)))
               (and data2
                    (call-candidate function
                                    (list port status data1 data2)))
               (error "Opusmodus ~A has an unsupported signature."
                      *opusmodus-midi-function-name*))))))))

(defun use-opusmodus-byte-midi ()
  (setf *midi-sender* (make-opusmodus-sender))
  :opusmodus-bytes)

(defun current-midi-output-port ()
  (let ((symbol (find-bound-symbol "*MIDI-OUTPUT-PORT*")))
    (unless symbol
      (error "Opusmodus *MIDI-OUTPUT-PORT* was not found."))
    (let ((value (symbol-value symbol)))
      (unless value
        (error "Opusmodus has no active MIDI output port."))
      value)))

(defun make-direct-midi-sender ()
  "Send CL-MIDI events to the destination named by each event's :PORT."
  (multiple-value-bind (find-destination)
      (require-function-in-package "FIND-MIDI-DESTINATION" "OPUSMODUS")
    (multiple-value-bind (make-event)
        (require-function-in-package "MAKE-MIDI-EVENT" "CL-MIDI")
      (multiple-value-bind (make-note-on)
          (require-function-in-package "MAKE-NOTE-ON-EVENT" "CL-MIDI")
        (multiple-value-bind (make-note-off)
            (require-function-in-package "MAKE-NOTE-OFF-EVENT" "CL-MIDI")
          (multiple-value-bind (send-event)
              (require-function-in-package "%SEND-MIDI-EVENT" "SEQUENCER")
            (let ((output-port (current-midi-output-port)))
              (clrhash *direct-midi-destinations*)
              (lambda (port status data1 data2)
                (unless port
                  (error "Direct MIDI requires an explicit :port."))
                (when (>= status #xF8)
                  (error "System real-time MIDI must use SEND-MIDI-REALTIME, not the CL-MIDI event path."))
                (let* ((destination
                         (or (gethash port *direct-midi-destinations*)
                             (setf (gethash port *direct-midi-destinations*)
                                   (or (funcall find-destination port)
                                       (error "MIDI destination ~S was not found."
                                              port)))))
                       (event-type (ldb (byte 4 4) status))
                       (channel (ldb (byte 4 0) status))
                       ;; Use CL-MIDI's dedicated note constructors. Besides
                       ;; documenting the intent, this avoids release-specific
                       ;; ambiguity in MAKE-MIDI-EVENT's numeric event type.
                       (event
                         (cond
                           ((= event-type #x9)
                            (funcall make-note-on 0 channel data1 data2))
                           ((= event-type #x8)
                            (funcall make-note-off 0 channel data1
                                     (or data2 0)))
                           (data2
                            (funcall make-event 0 event-type channel
                                     data1 data2))
                           (data1
                            (funcall make-event 0 event-type channel data1))
                           (t
                            (funcall make-event 0 event-type channel)))))
                  (funcall send-event event output-port destination))))))))))

(defun use-direct-midi ()
  "Select explicit per-event CoreMIDI routing through each :PORT value."
  (setf *midi-sender* (make-direct-midi-sender))
  :direct-midi)

(defun make-opusmodus-realtime-sender ()
  "Return the byte-level sender used only for one-byte real-time messages.

Opusmodus' %SEND-MIDI-BYTES correctly emits F8/FA/FC as single bytes. This
path intentionally does not use CL-MIDI:MAKE-MIDI-EVENT."
  (multiple-value-bind (function symbol)
      (opusmodus-function "%SEND-MIDI-BYTES")
    (unless function
      (error "Opusmodus function %SEND-MIDI-BYTES was not found."))
    ;; This function is called from the user's evaluation process, where
    ;; Opusmodus' dynamically bound MIDI output port is available. Preserve
    ;; those bindings for the dedicated clock process.
    (setf *opusmodus-midi-function-name* symbol
          *opusmodus-midi-bindings* (capture-opusmodus-midi-bindings))
    (lambda (port status)
      (declare (ignore port))
      (call-with-midi-bindings
       (lambda ()
         (funcall function (list status)))))))

(defun make-core-midi-packet-function ()
  "Compile a 1-3 byte CoreMIDI packet sender from Opusmodus' FLI macro."
  (let* ((opusmodus (or (find-package "OPUSMODUS")
                        (error "The OPUSMODUS package was not found.")))
         (fli (or (find-package "FLI")
                  (error "The LispWorks FLI package was not found.")))
         (with-packet-list
           (or (find-symbol "WITH-PACKET-LIST" opusmodus)
               (error "OPUSMODUS::WITH-PACKET-LIST was not found.")))
         (packet-length
           (or (find-symbol "PACKET-POINTER-LENGTH" opusmodus)
               (error "OPUSMODUS::PACKET-POINTER-LENGTH was not found.")))
         (packet-timestamp
           (or (find-symbol "PACKET-POINTER-TIMESTAMP" opusmodus)
               (error "OPUSMODUS::PACKET-POINTER-TIMESTAMP was not found.")))
         (midi-send
           (or (find-symbol "MIDI-SEND" opusmodus)
               (error "OPUSMODUS::MIDI-SEND was not found.")))
         (dereference
           (or (find-symbol "DEREFERENCE" fli)
               (error "FLI:DEREFERENCE was not found."))))
    (compile
     nil
     `(lambda (output-port destination status data1 data2 timestamp)
        (,with-packet-list (packet-list packet data)
          ;; Write all bytes through FLI:DEREFERENCE.  Some LispWorks builds
          ;; reject (:UNSIGNED :BYTE) in FOREIGN-TYPED-AREF even though the
          ;; same type is valid for byte pointers and DEREFERENCE.
          (setf (,packet-length packet) (cond (data2 3)
                                              (data1 2)
                                              (t 1))
                (,dereference data :type '(:unsigned :byte)) status)
          (when data1
            (setf (,dereference data :type '(:unsigned :byte) :index 1)
                  data1))
          (when data2
            (setf (,dereference data :type '(:unsigned :byte) :index 2)
                  data2))
          (when timestamp
            (setf (,packet-timestamp packet) timestamp))
          (,midi-send output-port destination packet-list))))))

(defun make-core-midi-bytes-packet-function ()
  "Compile a variable-length CoreMIDI packet sender from Opusmodus' FLI macro."
  (let* ((opusmodus (or (find-package "OPUSMODUS")
                        (error "The OPUSMODUS package was not found.")))
         (fli (or (find-package "FLI")
                  (error "The LispWorks FLI package was not found.")))
         (with-packet-list
           (or (find-symbol "WITH-PACKET-LIST" opusmodus)
               (error "OPUSMODUS::WITH-PACKET-LIST was not found.")))
         (packet-length
           (or (find-symbol "PACKET-POINTER-LENGTH" opusmodus)
               (error "OPUSMODUS::PACKET-POINTER-LENGTH was not found.")))
         (packet-timestamp
           (or (find-symbol "PACKET-POINTER-TIMESTAMP" opusmodus)
               (error "OPUSMODUS::PACKET-POINTER-TIMESTAMP was not found.")))
         (midi-send
           (or (find-symbol "MIDI-SEND" opusmodus)
               (error "OPUSMODUS::MIDI-SEND was not found.")))
         (dereference
           (or (find-symbol "DEREFERENCE" fli)
               (error "FLI:DEREFERENCE was not found."))))
    (compile
     nil
     `(lambda (output-port destination bytes timestamp)
        (,with-packet-list (packet-list packet data)
          (setf (,packet-length packet) (length bytes))
          (loop for byte in bytes
                for index from 0
                do (setf (,dereference data
                                      :type '(:unsigned :byte)
                                      :index index)
                         byte))
          (when timestamp
            (setf (,packet-timestamp packet) timestamp))
          (,midi-send output-port destination packet-list))))))

(defun make-core-midi-realtime-sender ()
  "Send F8/FA/FC directly to the destination named by PORT."
  (multiple-value-bind (find-destination)
      (require-function-in-package "FIND-MIDI-DESTINATION" "OPUSMODUS")
    (let ((output-port (current-midi-output-port))
          (packet-function (make-core-midi-packet-function))
          (destinations (make-hash-table :test #'equalp)))
      (lambda (port status &optional timestamp)
        (unless port
          (error "MIDI Clock requires an explicit :midi-clock-port."))
        (let ((destination
                (or (gethash port destinations)
                    (setf (gethash port destinations)
                          (or (funcall find-destination port)
                              (error "MIDI destination ~S was not found."
                                     port))))))
          (funcall packet-function output-port destination status
                   nil nil timestamp))))))

(defun make-core-midi-timestamped-sender ()
  "Send 1-3 raw MIDI bytes directly to PORT, optionally timestamped."
  (multiple-value-bind (find-destination)
      (require-function-in-package "FIND-MIDI-DESTINATION" "OPUSMODUS")
    (let ((output-port (current-midi-output-port))
          (packet-function (make-core-midi-packet-function))
          (destinations (make-hash-table :test #'equalp)))
      (lambda (port status data1 data2 &optional timestamp)
        (unless port
          (error "Timestamped MIDI requires an explicit :port."))
        (let ((destination
                (or (gethash port destinations)
                    (setf (gethash port destinations)
                          (or (funcall find-destination port)
                              (error "MIDI destination ~S was not found."
                                     port))))))
          (funcall packet-function output-port destination status
                   data1 data2 timestamp))))))

(defun make-core-midi-sysex-sender ()
  "Send variable-length SysEx packets directly to PORT, optionally timestamped."
  (multiple-value-bind (find-destination)
      (require-function-in-package "FIND-MIDI-DESTINATION" "OPUSMODUS")
    (let ((output-port (current-midi-output-port))
          (packet-function (make-core-midi-bytes-packet-function))
          (destinations (make-hash-table :test #'equalp)))
      (lambda (port bytes &optional timestamp)
        (unless port
          (error "SysEx MIDI requires an explicit :port."))
        (let ((destination
                (or (gethash port destinations)
                    (setf (gethash port destinations)
                          (or (funcall find-destination port)
                              (error "MIDI destination ~S was not found."
                                     port))))))
          (funcall packet-function output-port destination bytes timestamp))))))

(defun ensure-midi-realtime-sender ()
  (or *midi-realtime-sender*
      (setf *midi-realtime-sender*
            (make-opusmodus-realtime-sender))))

(defun prepare-midi-realtime-sender ()
  "Prepare explicit CoreMIDI routing before the clock thread starts."
  (setf *midi-realtime-sender* (make-core-midi-realtime-sender)
        *midi-realtime-sender-kind* :core-midi)
  *midi-realtime-sender*)

(defun prepare-midi-timestamped-sender ()
  "Prepare explicit timestampable CoreMIDI routing for musical events."
  (setf *midi-timestamped-sender*
        (make-core-midi-timestamped-sender)
        *midi-timestamped-sender-kind* :core-midi)
  *midi-timestamped-sender*)

(defun prepare-midi-sysex-sender ()
  "Prepare explicit timestampable CoreMIDI routing for SysEx/MTS events."
  (setf *midi-sysex-sender*
        (make-core-midi-sysex-sender)
        *midi-sysex-sender-kind* :core-midi)
  *midi-sysex-sender*)

(defun send-midi-realtime (port status &optional timestamp)
  "Send one system real-time byte without CL-MIDI event construction."
  (unless (member status '(#xF8 #xFA #xFB #xFC))
    (error "Unsupported MIDI real-time status ~X." status))
  (let ((sender (ensure-midi-realtime-sender)))
    (if timestamp
        (or (call-candidate sender (list port status timestamp))
            (call-candidate sender (list port status))
            (error "The MIDI real-time sender has an unsupported signature."))
        (funcall sender port status))))

(defun midi-realtime-scheduling-supported-p ()
  "True when real-time bytes can be timestamped with CoreMIDI host time."
  (handler-case
      (and (eq *midi-realtime-sender-kind* :core-midi)
           (core-midi-host-time)
           (core-midi-seconds-to-host-ticks 0.001d0)
           t)
    (error () nil)))

(defun diagnostic-value (thunk)
  (handler-case
      (list :ok (funcall thunk))
    (error (condition)
      (list :error (princ-to-string condition)))))

(defun midi-clock-diagnostics ()
  "Return diagnostics for the timestamped CoreMIDI clock path."
  (list :sender-kind *midi-realtime-sender-kind*
        :host-time
        (diagnostic-value #'core-midi-host-time)
        :timebase
        (diagnostic-value #'core-midi-host-timebase)
        :ticks-per-ms
        (diagnostic-value
         (lambda () (core-midi-seconds-to-host-ticks 0.001d0)))
        :scheduled-supported
        (midi-realtime-scheduling-supported-p)))

(defun send-midi-realtime-at-host-time (port status host-time)
  "Schedule one real-time byte at an absolute CoreMIDI host timestamp."
  (send-midi-realtime port status host-time))

(defun send-midi-realtime-after (port status seconds-from-now)
  "Schedule one real-time byte relative to the CoreMIDI host clock."
  (let ((timestamp
          (+ (core-midi-host-time)
             (core-midi-seconds-to-host-ticks seconds-from-now))))
    (send-midi-realtime port status timestamp)))

(defun midi-event-scheduling-supported-p ()
  "True when ordinary MIDI events can be timestamped with CoreMIDI host time."
  (handler-case
      (and (eq *midi-timestamped-sender-kind* :core-midi)
           (core-midi-host-time)
           (core-midi-seconds-to-host-ticks 0.001d0)
           t)
    (error () nil)))

(defun ensure-midi-timestamped-sender ()
  (or *midi-timestamped-sender*
      (prepare-midi-timestamped-sender)))

(defun send-midi-bytes-at-host-time (port status data1 data2 host-time)
  "Send a MIDI message at an absolute CoreMIDI host timestamp."
  (let ((sender (ensure-midi-timestamped-sender)))
    (funcall sender port
             (clamp-midi-status status)
             (and data1 (clamp-midi data1))
             (and data2 (clamp-midi data2))
             host-time)))

(defun ensure-midi-sysex-sender ()
  (or *midi-sysex-sender*
      (prepare-midi-sysex-sender)))

(defun clamp-midi-sysex-byte (value)
  (clamp-midi-status value))

(defun send-midi-sysex (port bytes &optional timestamp)
  "Send a complete SysEx byte list, including F0 and F7."
  (let ((sender (ensure-midi-sysex-sender)))
    (funcall sender port
             (mapcar #'clamp-midi-sysex-byte bytes)
             timestamp)))

(defun send-midi-sysex-at-host-time (port bytes host-time)
  "Send a complete SysEx byte list at an absolute CoreMIDI host timestamp."
  (send-midi-sysex port bytes host-time))

(defun use-opusmodus-midi ()
  "Compatibility name. The preferred Opusmodus backend now routes directly."
  (use-direct-midi))

(defun ensure-midi-sender ()
  (or *midi-sender*
      (if (and (opusmodus-function "FIND-MIDI-DESTINATION")
               (opusmodus-function "MAKE-MIDI-EVENT")
               (opusmodus-function "%SEND-MIDI-EVENT"))
          (use-direct-midi)
          (use-logging-midi))))

(defun send-midi-bytes (port status &optional data1 data2)
  (ensure-midi-sender)
  (funcall *midi-sender* port (clamp-midi-status status)
           (and data1 (clamp-midi data1))
           (and data2 (clamp-midi data2))))

(defun event-status-byte (event)
  (+ (ecase (midi-event-kind event)
       (:note-off #x80)
       (:note-on #x90)
       (:keyswitch-off #x80)
       (:keyswitch-on #x90)
       (:controller #xB0)
       (:articulation-controller #xB0)
       (:bank-select #xB0)
       (:program #xC0)
       (:articulation-program #xC0))
     (1- (midi-event-channel event))))

(defun dispatch-event (event)
  (let* ((kind (midi-event-kind event))
         (key (list (midi-event-port event)
                    (midi-event-channel event)
                    (midi-event-data1 event))))
    (if (eq kind :mts)
        (send-midi-sysex (midi-event-port event)
                         (midi-event-bytes event))
        (send-midi-bytes (midi-event-port event)
                         (event-status-byte event)
                         (midi-event-data1 event)
                         (unless (member kind '(:program :articulation-program))
                           (midi-event-data2 event))))
    (case kind
      ((:note-on :keyswitch-on)
       (setf (gethash key *active-notes*) t))
      ((:note-off :keyswitch-off)
       (remhash key *active-notes*)))))

(defun dispatch-event-at-host-time (event host-time)
  "Schedule EVENT with CoreMIDI timestamping and update Livecode note state."
  (let* ((kind (midi-event-kind event))
         (key (list (midi-event-port event)
                    (midi-event-channel event)
                    (midi-event-data1 event))))
    (if (eq kind :mts)
        (send-midi-sysex-at-host-time
         (midi-event-port event)
         (midi-event-bytes event)
         host-time)
        (send-midi-bytes-at-host-time
         (midi-event-port event)
         (event-status-byte event)
         (midi-event-data1 event)
         (unless (member kind '(:program :articulation-program))
           (midi-event-data2 event))
         host-time))
    (case kind
      ((:note-on :keyswitch-on)
       (setf (gethash key *active-notes*) t))
      ((:note-off :keyswitch-off)
       (remhash key *active-notes*)))))

(defun panic ()
  "Send note-offs for known active notes plus All Notes Off on all channels."
  (let (ports)
    (maphash
     (lambda (key value)
       (declare (ignore value))
       (pushnew (first key) ports :test #'equal))
     *active-notes*)
  (maphash
   (lambda (key value)
     (declare (ignore value))
     (destructuring-bind (port channel note) key
       (send-midi-bytes port (+ #x80 (1- channel)) note 0)))
   *active-notes*)
  (clrhash *active-notes*)
    (dolist (port ports)
      (dotimes (channel 16)
        (send-midi-bytes port (+ #xB0 channel) 123 0))))
  :ok)

(defun release-active-notes ()
  "Release only notes known to be active, preserving the MIDI routing."
  (let (keys)
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key keys))
             *active-notes*)
    (dolist (key keys)
      (destructuring-bind (port channel note) key
        (send-midi-bytes port (+ #x80 (1- channel)) note 0)
        (remhash key *active-notes*))))
  :ok)

(defun list-midi-destinations ()
  (let ((function (opusmodus-function "MIDI-DESTINATIONS")))
    (if function
        (funcall function)
        (error "MIDI-DESTINATIONS is only available inside Opusmodus."))))

(defun test-midi-output (&key (port "Bus 1") (channel 1)
                              (note 60) (velocity 100)
                              (duration 0.5))
  "Play one test note through the explicit direct-MIDI backend."
  (check-type channel (integer 1 16))
  (use-direct-midi)
  (send-midi-bytes port (+ #x90 (1- channel)) note velocity)
  (sleep duration)
  (send-midi-bytes port (+ #x80 (1- channel)) note 0)
  :ok)

(defun test-midi-timestamped-output (&key (port "Bus 1") (channel 1)
                                          (note 60) (velocity 100)
                                          (start-delay 0.10d0)
                                          (duration 0.30d0))
  "Play one test note using CoreMIDI timestamps for note-on and note-off.

Use this before enabling *TIMESTAMP-MIDI-EVENTS*: if this note sounds cleanly,
ordinary Livecode notes can be moved onto the same timestamped timing domain
as MIDI Clock."
  (check-type channel (integer 1 16))
  (unless (and (realp start-delay) (not (minusp start-delay)))
    (error "START-DELAY must be non-negative, got ~S." start-delay))
  (unless (and (realp duration) (plusp duration))
    (error "DURATION must be positive, got ~S." duration))
  (prepare-midi-timestamped-sender)
  (let* ((now (core-midi-host-time))
         (start-time
           (+ now (core-midi-seconds-to-host-ticks start-delay)))
         (stop-time
           (+ start-time (core-midi-seconds-to-host-ticks duration))))
    (send-midi-bytes-at-host-time port (+ #x90 (1- channel))
                                  note velocity start-time)
    (send-midi-bytes-at-host-time port (+ #x80 (1- channel))
                                  note 0 stop-time)
    (sleep (+ (coerce start-delay 'double-float)
              (coerce duration 'double-float)
              0.10d0)))
  :ok)

(defun test-midi-note-separation (&key (port "Bus 1") (channel 1)
                                       (notes '(60 64 67))
                                       (duration 0.2)
                                       (gap 0.3)
                                       (velocity 100))
  "Play monophonic notes with a silent gap after every explicit Note Off."
  (check-type channel (integer 1 16))
  (use-direct-midi)
  (dolist (note notes)
    (send-midi-bytes port (+ #x90 (1- channel)) note velocity)
    (sleep duration)
    (send-midi-bytes port (+ #x80 (1- channel)) note 0)
    (sleep gap))
  :ok)

(defun test-midi-clock-output (&key (port "Bus 2")
                                    (tempo 75)
                                    (seconds 2))
  "Send only MIDI Start, Clock and Stop directly to PORT."
  (unless (and (realp tempo) (plusp tempo))
    (error "Tempo must be positive, got ~S." tempo))
  (unless (and (realp seconds) (plusp seconds))
    (error "Seconds must be positive, got ~S." seconds))
  (prepare-midi-realtime-sender)
  (let* ((interval (/ 60d0 (* 24d0 (coerce tempo 'double-float))))
         (deadline (+ (monotonic-seconds)
                      (coerce seconds 'double-float)))
         (next-tick (monotonic-seconds))
         (count 0))
    (unwind-protect
         (progn
           (send-midi-realtime port #xFA)
           (loop while (< next-tick deadline)
                 do (precise-sleep-until next-tick)
                    (send-midi-realtime port #xF8)
                    (incf count)
                    ;; As in the live clock, never shorten the next interval
                    ;; to compensate for a late dispatch.
                    (setf next-tick
                          (+ (monotonic-seconds) interval))))
      (send-midi-realtime port #xFC))
    (list :port port :tempo tempo :ticks count :seconds seconds)))
