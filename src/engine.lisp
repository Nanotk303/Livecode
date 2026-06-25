(in-package #:livecode)

(defparameter *retrigger-note-off-advance-beats* 1/256
  "Move a note-off slightly before a same-pitch retrigger on the same track.

This prevents exact note-off/note-on collisions for repeated drums such as a
kick on every quarter note, without shortening unrelated sustained material.")

(defparameter *mts-enabled* t
  "When true, fractional MIDI pitches emit MIDI Tuning Standard SysEx events.")

(defparameter *mts-device-id* #x7F
  "MTS SysEx device id. #x7F is the Universal SysEx all-call device id.")

(defparameter *mts-tuning-program* 0
  "MTS tuning program number used for Single-note Tuning Change messages.")

(defparameter *mts-lead-time-seconds* 0.005d0
  "How far before a note-on its MTS retuning SysEx is sent.")

(defparameter *sound-set-articulations-enabled* t
  "When true, symbolic OMN articulations can trigger Opusmodus sound-set events.")

(defparameter *sound-set-program-resolver* nil
  "Optional resolver used by tests or extensions.

When NIL, Livecode asks Opusmodus' GET-SOUND-SET-PROGRAM directly.")

(defparameter *keyswitch-lead-time-seconds* 0.030d0
  "How far before the musical note sound-set keyswitch note-ons are sent.")

(defparameter *keyswitch-duration-seconds* 0.020d0
  "Duration of generated sound-set keyswitch notes.")

(defparameter *keyswitch-velocity* 100
  "Velocity used for generated sound-set keyswitch note-ons.")

(defparameter *sound-set-message-lead-time-seconds* 0.030d0
  "How far before the musical note articulation CC/program/bank messages are sent.")

(defparameter *send-redundant-articulation-messages* nil
  "When NIL, sound-set articulation messages are sent only when articulation changes.

Set this to T for instruments that need keyswitch/CC/program state reasserted
on every note.")

(defparameter *event-lateness-warning-seconds* 0.003d0
  "Scheduler lateness threshold counted by LIVE-STATUS on the direct MIDI path.

This does not affect playback. It is a diagnostic for audible timing slips in
the non-timestamped scheduler.")

(defvar *event-late-count* 0)
(defvar *event-max-lateness-seconds* 0d0)

(defun reset-event-lateness-stats ()
  (setf *event-late-count* 0
        *event-max-lateness-seconds* 0d0))

(defun record-event-lateness (deadline)
  (let ((lateness (max 0d0 (- (monotonic-seconds) deadline))))
    (when (> lateness *event-lateness-warning-seconds*)
      (incf *event-late-count*))
    (when (> lateness *event-max-lateness-seconds*)
      (setf *event-max-lateness-seconds* lateness))
    lateness))

(defun controller-step-p (value)
  (and (consp value)
       (numberp (first value))
       (numberp (second value))))

(defun resolve-controller-source (source)
  (loop for value = source then resolved
        for resolved = (resolve-source value)
        until (eq value resolved)
        finally (return resolved)))

(defun controller-steps (values)
  "Return timed controller steps of the shape (value duration).

Nested phrase/bar lists are flattened, but a two-number leaf is preserved as a
single automation step."
  (labels ((walk (node)
             (cond
               ((null node) nil)
               ((controller-step-p node) (list node))
               ((consp node) (mapcan #'walk node))
               (t nil))))
    (walk (resolve-controller-source values))))

(defun controller-static-value (values)
  (find-if #'numberp
           (alexandria-free-flatten
            (resolve-controller-source values))))

(defun resolve-controller-number (sound controller)
  (cond
    ((numberp controller) controller)
    ((and sound (symbolp controller))
     (or (registry-sound-set-controller sound controller)
         (let ((resolver (opusmodus-function "GET-SOUND-SET-CONTROLLER")))
           (and resolver
                (ignore-errors
                  (funcall resolver (unquote sound) (unquote controller)))))
         (error "Unknown sound-set controller ~S for sound set ~S."
                controller sound)))
    (t
     (error "Controller name must be a MIDI number or a sound-set controller symbol, got ~S."
            controller))))

(defun controller-values-to-events (number values port channel track &key sound)
  (let ((steps (controller-steps values))
        (number (resolve-controller-number sound number))
        (events nil)
        (beat 0))
    (if steps
        (dolist (step steps)
          (destructuring-bind (value duration &rest ignored) step
            (declare (ignore ignored))
            (push (make-midi-event
                   :beat beat :kind :controller :port port
                   :channel channel :data1 (clamp-midi number)
                   :data2 (clamp-midi value) :track track)
                  events)
            (incf beat
                  (abs (opusmodus-length-to-beats duration)))))
        (let ((value (controller-static-value values)))
          (when value
            (push (make-midi-event
                   :beat 0 :kind :controller :port port
                   :channel channel :data1 (clamp-midi number)
                   :data2 (clamp-midi value) :track track)
                  events))))
    (values (nreverse events) beat)))

(defun controllers-to-events (controllers port channel track &key sound)
  (let ((controllers (resolve-controller-source controllers))
        (events nil)
        (length 0))
    (loop for (number values) on controllers by #'cddr
          while values
          do (multiple-value-bind (controller-events controller-length)
                 (controller-values-to-events number values port channel track
                                              :sound sound)
               (setf events (nconc events controller-events)
                     length (max length controller-length))))
    (values events length)))

(defun pitch-list (pitch)
  (if (listp pitch) pitch (list pitch)))

(defun integer-midi-pitch-p (pitch)
  (and (realp pitch)
       (< (abs (- (coerce pitch 'double-float)
                  (round pitch)))
          0.000001d0)))

(defun microtonal-pitch-p (pitch)
  (and *mts-enabled*
       (realp pitch)
       (not (integer-midi-pitch-p pitch))))

(defun midi-key-for-pitch (pitch)
  (clamp-midi (if (microtonal-pitch-p pitch)
                  (floor pitch)
                  pitch)))

(defun mts-frequency-bytes (pitch)
  "Return the 3-byte MTS frequency word for fractional MIDI pitch PITCH."
  (let* ((bounded (max 0d0 (min 127.99993896484375d0
                                (coerce pitch 'double-float))))
         (semitone (floor bounded))
         (fraction (- bounded semitone))
         (fraction-14 (round (* fraction 16384d0))))
    (when (= fraction-14 16384)
      (incf semitone)
      (setf fraction-14 0))
    (setf semitone (max 0 (min 127 semitone)))
    (list semitone
          (ldb (byte 7 7) fraction-14)
          (ldb (byte 7 0) fraction-14))))

(defun mts-single-note-tuning-bytes (key pitch)
  "Build a real-time MTS Single-note Tuning Change SysEx for KEY -> PITCH."
  (destructuring-bind (semitone fraction-msb fraction-lsb)
      (mts-frequency-bytes pitch)
    (list #xF0 #x7F (clamp-midi *mts-device-id*) #x08 #x02
          (clamp-midi *mts-tuning-program*) 1
          (clamp-midi key) semitone fraction-msb fraction-lsb #xF7)))

(defun maybe-mts-event (beat pitch port channel track)
  (when (microtonal-pitch-p pitch)
    (let ((key (midi-key-for-pitch pitch)))
      (make-midi-event :beat beat :kind :mts :port port
                       :channel channel :data1 key :data2 0
                       :bytes (mts-single-note-tuning-bytes key pitch)
                       :track track))))

(defun sound-set-program-value (sound program)
  (when (and *sound-set-articulations-enabled* sound program)
    (let ((sound (unquote sound))
          (program (unquote program)))
      (handler-case
          (cond
            (*sound-set-program-resolver*
             (funcall *sound-set-program-resolver* sound program))
            ((registry-sound-set-program sound program))
            (t
             (let ((resolver (opusmodus-function "GET-SOUND-SET-PROGRAM")))
               (and resolver
                    (funcall resolver sound program)))))
        (error () nil)))))

(defun split-symbol-name (symbol delimiter)
  (let ((name (symbol-name symbol))
        (start 0)
        parts)
    (loop for index = (cl:position delimiter name :start start)
          do (push (subseq name start index) parts)
          while index
          do (setf start (1+ index))
          finally (return (nreverse parts)))))

(defun symbol-like (prototype name)
  (intern name (symbol-package prototype)))

(defun articulation-alternatives (articulation)
  "Return ARTICULATION and safe spelling fallbacks.

Several Opusmodus soundsets name compound articulations as A+B, while musical
material may naturally use B+A.  Try the exact symbol first, then the reversed
compound spelling."
  (let ((articulation (unquote articulation)))
    (if (and (symbolp articulation)
             (find #\+ (symbol-name articulation)))
        (let ((parts (split-symbol-name articulation #\+)))
          (remove-duplicates
           (list articulation
                 (symbol-like articulation
                              (format nil "~{~A~^+~}" (reverse parts))))
           :test #'string-equal
           :key #'symbol-name))
        (list articulation))))

(defun resolve-sound-set-articulation (sound articulation)
  (loop for candidate in (articulation-alternatives articulation)
        for value = (sound-set-program-value sound candidate)
        when value
          return value))

(defun resolve-sound-set-articulation-with-name (sound articulation)
  (loop for candidate in (articulation-alternatives articulation)
        for value = (sound-set-program-value sound candidate)
        when value
          return (values value candidate)))

(defun default-articulation-from-program (program)
  (let ((program (unquote program)))
    (cond
      ((symbolp program) program)
      ((and (consp program)
            (symbolp (first program)))
       (first program))
      (t nil))))

(defun articulation-list (articulation)
  (let ((articulation (unquote articulation)))
    (cond
      ((null articulation) nil)
      ((and (consp articulation)
            (eq (first articulation) 'quote))
       (articulation-list (second articulation)))
      ((consp articulation) articulation)
      (t (list articulation)))))

(defun bank-select-events (beat bank port channel track &key articulation-p)
  (let ((kind (if articulation-p :bank-select :controller)))
    (cond
      ((and (integerp bank) (<= 0 bank 127))
       (list (make-midi-event :beat beat :kind kind :port port
                              :channel channel :data1 0
                              :data2 bank :track track)))
      ((and (integerp bank) (<= 0 bank 16383))
       (list (make-midi-event :beat beat :kind kind :port port
                              :channel channel :data1 0
                              :data2 (ldb (byte 7 7) bank) :track track)
             (make-midi-event :beat beat :kind kind :port port
                              :channel channel :data1 32
                              :data2 (ldb (byte 7 0) bank) :track track)))
      ((and (consp bank) (every #'numberp bank))
       (loop for controller in '(0 32)
             for value in bank
             collect (make-midi-event :beat beat :kind kind :port port
                                      :channel channel
                                      :data1 (clamp-midi controller)
                                      :data2 (clamp-midi value)
                                      :track track)))
      (t
       (error "Unsupported bank-select value ~S." bank)))))

(defun sound-set-program-events (beat program port channel track
                                      &key articulation-p)
  (list (make-midi-event :beat beat
                         :kind (if articulation-p
                                   :articulation-program
                                   :program)
                         :port port :channel channel
                         :data1 (clamp-midi program)
                         :data2 0 :track track)))

(defun sound-set-controller-event (beat sound controller value
                                        port channel track)
  (make-midi-event :beat beat :kind :articulation-controller
                   :port port :channel channel
                   :data1 (clamp-midi (resolve-controller-number
                                       sound controller))
                   :data2 (clamp-midi value)
                   :track track))

(defun sound-set-articulation-value-events
    (value beat sound port channel track)
  "Compile one sound-set articulation/program value into MIDI events."
  (cond
    ((null value) nil)
    ((numberp value)
     (sound-set-program-events beat value port channel track
                               :articulation-p t))
    ((and (consp value)
          (= (length value) 2)
          (every #'numberp value))
     (append (bank-select-events beat (first value) port channel track
                                 :articulation-p t)
             (sound-set-program-events beat (second value) port channel track
                                       :articulation-p t)))
    ((consp value)
     (let ((events nil)
           (rest value))
       (labels ((emit (event) (push event events)))
         (loop while rest
               for item = (pop rest)
               do (cond
                    ((and (symbolp item)
                          (string-equal (symbol-name item) "KEY"))
                     (let* ((key (pop rest))
                            (midi (and (symbolp key)
                                       (parse-pitch-symbol key))))
                       (unless midi
                         (error "Could not parse sound-set keyswitch pitch ~S."
                                key))
                       (emit (make-midi-event
                              :beat beat :kind :keyswitch-on
                              :port port :channel channel
                              :data1 (clamp-midi midi)
                              :data2 (clamp-midi *keyswitch-velocity*)
                              :track track))
                       (emit (make-midi-event
                              :beat beat :kind :keyswitch-off
                              :port port :channel channel
                              :data1 (clamp-midi midi)
                              :data2 0 :track track))))
                    ((and (symbolp item)
                          (member (string-upcase (symbol-name item))
                                  '("PROGRAM" "PC") :test #'string=))
                     (emit (first (sound-set-program-events
                                   beat (pop rest) port channel track
                                   :articulation-p t))))
                    ((and (symbolp item)
                          (member (string-upcase (symbol-name item))
                                  '("BANK" "BANK-SELECT") :test #'string=))
                     (dolist (event (bank-select-events
                                     beat (pop rest) port channel track
                                     :articulation-p t))
                       (emit event)))
                    ((numberp item)
                     (emit (first (sound-set-program-events
                                   beat item port channel track
                                   :articulation-p t))))
                    ((and rest (numberp (first rest)))
                     (emit (sound-set-controller-event
                            beat sound item (pop rest)
                            port channel track)))
                    (t
                     (error "Unsupported sound-set articulation item ~S in ~S."
                            item value)))))
       (nreverse events)))
    (t nil)))

(defun sound-set-articulation-events (beat sound articulation port channel track)
  (loop for one-articulation in (articulation-list articulation)
        for value = (resolve-sound-set-articulation sound one-articulation)
        append (sound-set-articulation-value-events
                value beat sound port channel track)))

(defun sound-set-key-switch-pitches (sound articulation)
  "Return MIDI key numbers encoded by an Opusmodus sound-set articulation.

This covers the VSL style used in the supplied files:
  def (:key cs1 :key c2 ...)
"
  (loop for event in (sound-set-articulation-events
                      0 sound articulation nil 1 nil)
        when (eq (midi-event-kind event) :keyswitch-on)
          collect (midi-event-data1 event)))

(defun keyswitch-events (beat sound articulation port channel track)
  (sound-set-articulation-events beat sound articulation port channel track))

(defun keyswitch-pitch-names (sound articulation)
  (loop for one-articulation in (articulation-list articulation)
        for value = (resolve-sound-set-articulation sound one-articulation)
        append
        (loop for (marker key) on value by #'cddr
              when (and (eq marker :key)
                        (symbolp key))
                collect key)))

(defun articulation-event-summary (event)
  (list :beat (midi-event-beat event)
        :kind (midi-event-kind event)
        :channel (midi-event-channel event)
        :data1 (midi-event-data1 event)
        :data2 (midi-event-data2 event)
        :track (midi-event-track event)))

(defun note-articulation-summary (note sound default-articulation)
  (destructuring-bind (beat duration pitch velocity articulation) note
    (let* ((effective (or articulation default-articulation))
           (raw
             (and effective
                  (loop for one-articulation in (articulation-list effective)
                        collect (list one-articulation
                                      (resolve-sound-set-articulation
                                       sound one-articulation)))))
           (keys (and effective
                      (keyswitch-pitch-names sound effective)))
           (messages (and effective
                          (mapcar #'articulation-event-summary
                                  (sound-set-articulation-events
                                   beat sound effective nil 1 nil))))
           (midi-keys (and effective
                           (sound-set-key-switch-pitches sound effective))))
      (list :beat beat
            :duration duration
            :pitch pitch
            :velocity velocity
            :articulation articulation
            :effective-articulation effective
            :soundset-raw raw
            :messages messages
            :keyswitches keys
            :keyswitch-midi midi-keys))))

(defun inspect-track-articulations (form)
  (destructuring-bind (name &rest options) form
    (let* ((source (getf options :omn))
           (sound (getf options :sound))
           (program (getf options :program))
           (default-articulation
             (default-articulation-from-program program)))
      (multiple-value-bind (notes length) (compile-omn-for-track source)
        (list :track name
              :sound (unquote sound)
              :program (unquote program)
              :default-articulation default-articulation
              :loop-beats length
              :notes (mapcar (lambda (note)
                               (note-articulation-summary
                                note sound default-articulation))
                             notes))))))

(defun same-effective-articulation-p (left right)
  (equal (mapcar (lambda (value)
                   (if (symbolp value)
                       (string-upcase (symbol-name value))
                       value))
                 (articulation-list left))
         (mapcar (lambda (value)
                   (if (symbolp value)
                       (string-upcase (symbol-name value))
                       value))
                 (articulation-list right))))

(defun articulation-messages-needed-p (articulation previous-articulation)
  (and articulation
       (or *send-redundant-articulation-messages*
           (not (same-effective-articulation-p articulation
                                               previous-articulation)))))

(defun note-contains-pitch-p (note pitch)
  (member pitch (pitch-list (third note)) :test #'=))

(defun same-pitch-retrigger-at-p (notes beat pitch &optional loop-length)
  (or (some (lambda (note)
              (and (= (first note) beat)
                   (note-contains-pitch-p note pitch)))
            notes)
      (and loop-length
           (= beat loop-length)
           (some (lambda (note)
                   (and (zerop (first note))
                        (note-contains-pitch-p note pitch)))
                 notes))))

(defun note-off-beat (notes beat duration pitch loop-length gate-beats)
  (let ((end (+ beat (if (and gate-beats (plusp gate-beats))
                         (min duration gate-beats)
                         duration))))
    (if (and (plusp *retrigger-note-off-advance-beats*)
             (same-pitch-retrigger-at-p notes end pitch loop-length))
        (max beat
             (- end
                (min *retrigger-note-off-advance-beats*
                     (/ duration 2))))
        end)))

(defun compile-track (form)
  (destructuring-bind (name &rest options) form
    (let* ((source (getf options :omn))
           (port (getf options :port))
           (channel (or (getf options :channel) 1))
           (sound (getf options :sound))
           (program (getf options :program))
           (controllers (getf options :controllers))
           (gate-beats (or (getf options :gate-beats)
                           (getf options :gate)))
           (default-articulation
             (default-articulation-from-program program))
           events)
      (check-type channel (integer 1 16))
      (when gate-beats
        (unless (and (realp gate-beats) (plusp gate-beats))
          (error "Track :GATE/:GATE-BEATS must be a positive beat count, got ~S."
                 gate-beats)))
      (multiple-value-bind (notes length) (compile-omn-for-track source)
        (when (numberp program)
          (push (make-midi-event :beat 0 :kind :program :port port
                                 :channel channel :data1 (clamp-midi program)
                                 :data2 0 :track name)
                events))
        (multiple-value-bind (controller-events controller-length)
            (controllers-to-events controllers port channel name
                                   :sound sound)
          (setf events (nconc controller-events events)
                length (max length controller-length)))
        (loop with previous-articulation = nil
              for note in notes
              do
          (destructuring-bind (beat duration pitch velocity articulation) note
            (let ((effective-articulation
                    (or articulation default-articulation)))
              (when (articulation-messages-needed-p
                     effective-articulation previous-articulation)
                (dolist (keyswitch
                         (reverse
                          (keyswitch-events
                           beat sound effective-articulation
                           port channel name)))
                  (push keyswitch events)))
              (setf previous-articulation effective-articulation))
            (dolist (one-pitch (pitch-list pitch))
              (let ((mts-event
                      (maybe-mts-event beat one-pitch port channel name)))
                (when mts-event
                  (push mts-event events)))
              (push (make-midi-event :beat beat :kind :note-on :port port
                                     :channel channel
                                     :data1 (midi-key-for-pitch one-pitch)
                                     :data2 (clamp-midi velocity) :track name)
                    events)
              (push (make-midi-event :beat (note-off-beat notes beat duration
                                                          one-pitch length
                                                          gate-beats)
                                     :kind :note-off :port port
                                     :channel channel
                                     :data1 (midi-key-for-pitch one-pitch)
                                     :data2 0 :track name)
                    events))))
        (values events length)))))

(defun event-priority (event)
  (ecase (midi-event-kind event)
    (:keyswitch-on 0)
    (:mts 1)
    (:bank-select 2)
    (:articulation-controller 3)
    (:articulation-program 4)
    (:program 5)
    (:note-off 6)
    (:note-on 7)
    (:controller 8)
    (:keyswitch-off 9)))

(defun compile-scene (tracks &key (tempo 120) midi-clock-port
                                   link
                                   (link-quantum *ableton-link-default-quantum*)
                                   (link-start-stop
                                    *ableton-link-start-stop-sync*))
  "Compile the quoted instrument forms accepted by LIVE."
  (unless (and (realp tempo) (plusp tempo))
    (error "Tempo must be a positive number, got ~S." tempo))
  (when link
    (unless (and (realp link-quantum) (plusp link-quantum))
      (error "Link quantum must be a positive beat count, got ~S."
             link-quantum)))
  (let (events (length 0))
    (dolist (track (resolve-source tracks))
      (multiple-value-bind (track-events track-length)
          (compile-track track)
        (setf events (nconc track-events events)
              length (max length track-length))))
    (when (zerop length)
      (setf length 4))
    (setf events
          (stable-sort events
                       (lambda (left right)
                         (or (< (midi-event-beat left)
                                (midi-event-beat right))
                             (and (= (midi-event-beat left)
                                     (midi-event-beat right))
                                  (< (event-priority left)
                                     (event-priority right)))))))
    (make-scene :events events :length length :tempo tempo
                :midi-clock-port midi-clock-port
                :link-enabled link
                :link-quantum link-quantum
                :link-start-stop link-start-stop
                :source tracks)))

(defun seconds-per-beat (scene)
  (/ 60d0 (coerce (scene-tempo scene) 'double-float)))

(defun scene-duration-seconds (scene)
  (* (seconds-per-beat scene)
     (coerce (scene-length scene) 'double-float)))

(defun engine-live-p (engine)
  "Return true when ENGINE is still usable and running.

Guard every accessor: LispWorks keeps old DEFSTRUCT instances alive after a
reload, and accessing any slot on such an obsolete ENGINE can signal an error.
"
  (handler-case
      (and engine
           (engine-running-p engine)
           (or (null (engine-thread engine))
               (thread-alive-p (engine-thread engine))))
    (error () nil)))

(defun engine-last-error-safe (engine)
  (and engine
       (ignore-errors (engine-last-error engine))))

(defun quantization-beats (quantization engine scene)
  (cond ((eq quantization :beat) 1)
        ((eq quantization :bar) (engine-beats-per-bar engine))
        ((eq quantization :cycle) (scene-length scene))
        ((and (realp quantization) (plusp quantization)) quantization)
        (t nil)))

(defun next-quantized-time (engine quantization now)
  (let* ((scene (engine-current-scene engine))
         (origin (engine-cycle-start-time engine)))
    (cond
      ((eq quantization :immediate) now)
      (t
       (let ((beats (quantization-beats quantization engine scene)))
         (unless beats
           (error "Quantization must be :IMMEDIATE, :BEAT, :BAR, :CYCLE or a positive beat count, got ~S."
                  quantization))
         (if (scene-link-enabled scene)
             (progn
               (ensure-ableton-link-loaded)
               (ableton-link-next-quantized-time beats now))
             (let* ((quantum-seconds (* beats (seconds-per-beat scene)))
                    (elapsed (max 0d0 (- now origin)))
                    (index (ceiling (/ elapsed quantum-seconds)))
                    (target (+ origin (* index quantum-seconds))))
               (if (< target (- now 0.001d0))
                   (+ target quantum-seconds)
                   target))))))))

(defun pending-swap-time (engine)
  (with-engine-lock (engine)
    (and (engine-pending-scene engine)
         (engine-pending-start-time engine))))

(defun event-at-or-after-pending-swap-p (engine deadline)
  (let ((swap-time (pending-swap-time engine)))
    (and swap-time
         (<= swap-time deadline))))

(defparameter *event-wakeup-resolution-seconds* 0.001d0
  "Maximum scheduler sleep chunk for the non-timestamped event path.

This is intentionally much shorter than the old 10 ms chunk: sixteenth-note
streams expose even small wake-up jitter. The final millisecond is handled by
PRECISE-SLEEP-UNTIL.")

(defun wait-until-event-or-swap (engine deadline)
  (loop
    (unless (engine-live-p engine)
      (return :stopped))
    (let* ((now (monotonic-seconds))
           (swap-time (pending-swap-time engine)))
      (when (and swap-time
                 (<= swap-time deadline)
                 (<= swap-time now))
        (return :swap))
      (when (>= now deadline)
        (return :event))
      (let* ((next-interest (if swap-time
                                (min deadline swap-time)
                                deadline))
             (remaining (- next-interest now)))
        (if (<= remaining 0.0015d0)
            (precise-sleep-until next-interest)
            (sleep (min *event-wakeup-resolution-seconds*
                        (- remaining 0.001d0))))))))

(defun wait-until-pending-swap (engine)
  (let ((swap-time (pending-swap-time engine)))
    (if swap-time
        (wait-until-event-or-swap engine swap-time)
        :event)))

(defparameter *event-schedule-ahead-seconds* 0.25d0
  "How far ordinary MIDI events are timestamped ahead of playback.

Keep this short: scheduled CoreMIDI packets cannot be recalled if a live-code
replacement arrives just after they are queued.")

(defparameter *timestamped-swap-safety-margin-seconds* 0.02d0
  "Extra delay added to live-code swaps in timestamped mode.

CoreMIDI events already queued inside *EVENT-SCHEDULE-AHEAD-SECONDS* cannot be
cancelled.  The margin keeps a new scene from starting exactly on top of the
tail of the previous scene's queued window.")

(defparameter *timestamp-midi-events* nil
  "Experimental. When true, schedule ordinary MIDI events with CoreMIDI
timestamps. NIL keeps the proven direct event path while MIDI Clock can still
use timestamped CoreMIDI.")

(defun timestamped-safe-swap-request-time (now)
  (if *timestamp-midi-events*
      (+ now
         (coerce *event-schedule-ahead-seconds* 'double-float)
         (coerce *timestamped-swap-safety-margin-seconds* 'double-float))
      now))

(defun next-live-swap-time (engine quantization now)
  "Return a replacement time that cannot overlap already queued MIDI events."
  (next-quantized-time engine quantization
                       (timestamped-safe-swap-request-time now)))

(defparameter *midi-clock-start-delay-seconds* 0.08d0
  "Pre-roll used when starting MIDI Clock.

The clock START is timestamped slightly in the future; the musical cycle is
aligned to the first following F8 tick so clock-driven arpeggiators and
Livecode notes share one phase origin.")

(defparameter *midi-clock-event-offset-seconds* 0d0
  "Offset ordinary Livecode note events when MIDI Clock is active.

Positive values delay Livecode notes relative to the MIDI Clock grid; negative
values advance them. This is a tiny calibration trim for external hosts or
clock-driven arpeggiators that do not sound exactly on the received clock edge.")

(defun clock-compensated-event-p (scene event)
  (and (scene-midi-clock-port scene)
       (member (midi-event-kind event)
               '(:note-on :note-off :mts :keyswitch-on :keyswitch-off
                 :bank-select :articulation-controller
                 :articulation-program))))

(defun event-deadline (scene event cycle-start seconds-per-beat)
  (+ cycle-start
     (* seconds-per-beat
        (coerce (midi-event-beat event) 'double-float))
     (if (clock-compensated-event-p scene event)
         (coerce *midi-clock-event-offset-seconds* 'double-float)
         0d0)
     (if (eq (midi-event-kind event) :mts)
         (- (coerce *mts-lead-time-seconds* 'double-float))
         0d0)
     (ecase (midi-event-kind event)
       (:keyswitch-on
        (- (coerce *keyswitch-lead-time-seconds* 'double-float)))
       (:keyswitch-off
        (coerce *keyswitch-duration-seconds* 'double-float))
       ((:bank-select :articulation-controller :articulation-program)
        (- (coerce *sound-set-message-lead-time-seconds* 'double-float)))
       ((:note-on :note-off :controller :program :mts)
        0d0))))

(defun same-deadline-p (left right)
  (< (abs (- left right)) 0.000000001d0))

(defun initial-event-p (event)
  (zerop (midi-event-beat event)))

(defun scene-initial-events (scene)
  (remove-if-not #'initial-event-p (scene-events scene)))

(defun scene-events-after-initial (scene)
  (remove-if #'initial-event-p (scene-events scene)))

(defun next-event-group (events scene cycle-start seconds-per-beat)
  "Return events sharing the next exact playback deadline.

The scene is already sorted by beat and priority. Grouping avoids repeated
scheduler wakeups for simultaneous multi-channel events, which could make a
kick or another transient sound late merely because it appeared later in the
event list."
  (when events
    (let* ((deadline (event-deadline scene (first events) cycle-start
                                     seconds-per-beat))
           (group nil)
           (rest events))
      (loop while (and rest
                       (same-deadline-p
                        deadline
                        (event-deadline scene (first rest) cycle-start
                                        seconds-per-beat)))
            do (push (pop rest) group))
      (values (nreverse group) deadline rest))))

(defun dispatch-event-group (events)
  (dolist (event events)
    (dispatch-event event)))

(defun dispatch-event-group-at-host-time (events host-time)
  (dolist (event events)
    (dispatch-event-at-host-time event host-time)))

(defun event-host-time (deadline)
  (+ (core-midi-host-time)
     (core-midi-seconds-to-host-ticks
      (max 0.001d0 (- deadline (monotonic-seconds))))))

(defun note-event-identity (event)
  (list (midi-event-track event)
        (midi-event-port event)
        (midi-event-channel event)
        (midi-event-data1 event)))

(defun matching-note-off-p (note-on event)
  (and (eq (midi-event-kind event) :note-off)
       (equal (note-event-identity note-on)
              (note-event-identity event))))

(defun find-matching-note-off (note-on future-events)
  (find-if (lambda (event)
             (matching-note-off-p note-on event))
           future-events))

(defun matching-note-on-at-beat-p (note-off events beat)
  (some (lambda (event)
          (and (eq (midi-event-kind event) :note-on)
               (= (midi-event-beat event) beat)
               (equal (note-event-identity note-off)
                      (note-event-identity event))))
        events))

(defun safe-paired-note-off-p (note-off future-events scene
                                        &optional cycle-start seconds-per-beat
                                          cutoff-time)
  "True when NOTE-OFF can be queued separately from its musical group.

Do not pre-pair a note-off that lands exactly on a same-note retrigger.  MIDI
packets with identical CoreMIDI timestamps may not preserve the order in which
separate packets were queued, and a note-off after the new note-on can suppress
the transient.  Such retrigger note-offs must stay in the normal event group,
where Livecode orders note-off before note-on."
  (and note-off
       (let ((beat (midi-event-beat note-off)))
         (and
          (not (or (matching-note-on-at-beat-p note-off future-events beat)
                   (and (= beat (scene-length scene))
                        (matching-note-on-at-beat-p
                         note-off (scene-initial-events scene) 0))))
          (or (null cutoff-time)
              (< (event-deadline scene note-off cycle-start seconds-per-beat)
                 cutoff-time))))))

(defun schedule-note-off-with-note-on (note-on note-off scene cycle-start
                                               seconds-per-beat)
  "Timestamp NOTE-OFF when NOTE-ON is queued.

CoreMIDI packets already queued ahead of a live-code swap cannot be cancelled.
If the note-on is queued but the scheduler later swaps before reaching the
  note-off, the instrument can otherwise be left sustaining.  Pairing the
  note-off with the note-on makes queued notes self-contained."
  (declare (ignore note-on))
  (when note-off
    (let* ((deadline (event-deadline scene note-off cycle-start
                                     seconds-per-beat))
           (host-time (event-host-time deadline)))
      (dispatch-event-at-host-time note-off host-time))))

(defun dispatch-event-group-at-host-time-with-paired-note-offs
    (events future-events scene cycle-start seconds-per-beat host-time
     &optional cutoff-time)
  (let ((paired-note-offs nil))
    (dispatch-event-group-at-host-time events host-time)
    (dolist (event events)
      (when (eq (midi-event-kind event) :note-on)
        (let ((note-off (find-matching-note-off event future-events)))
          (when (safe-paired-note-off-p note-off future-events scene
                                        cycle-start seconds-per-beat
                                        cutoff-time)
            (push note-off paired-note-offs)
            (schedule-note-off-with-note-on event note-off scene cycle-start
                                            seconds-per-beat)))))
    paired-note-offs))

(defun remove-paired-note-offs (events paired-note-offs)
  (if paired-note-offs
      (remove-if (lambda (event)
                   (member event paired-note-offs :test #'eq))
                 events)
      events))

(defun take-pending-scene (engine)
  (with-engine-lock (engine)
    (when (and (engine-pending-scene engine)
               (<= (engine-pending-start-time engine)
                   (monotonic-seconds)))
      (multiple-value-prog1
          (values (engine-pending-scene engine)
                  (engine-pending-start-time engine))
        (setf (engine-pending-scene engine) nil
              (engine-pending-start-time engine) nil)))))

(defun run-one-cycle-immediate (engine scene cycle-start skip-initial-p)
  (let* ((seconds-per-beat (seconds-per-beat scene))
         (next-start
           (+ cycle-start
              (* seconds-per-beat
                 (coerce (scene-length scene) 'double-float)))))
    (loop with events = (if skip-initial-p
                            (scene-events-after-initial scene)
                            (scene-events scene))
          while events
          do (multiple-value-bind (group deadline rest)
                 (next-event-group events scene cycle-start seconds-per-beat)
               (let ((result (wait-until-event-or-swap engine deadline)))
                 (case result
                   (:swap (return-from run-one-cycle-immediate
                            (values :swap nil)))
                   (:stopped (return-from run-one-cycle-immediate
                              (values :stopped nil)))
                   (:event
                    (record-event-lateness deadline)
                    (dispatch-event-group group)
                    (when (same-deadline-p deadline next-start)
                      ;; Avoid a cycle-boundary hiccup: send the next cycle's
                      ;; beat-0 events in the same scheduler wakeup as the
                      ;; current cycle's boundary events, then ask the next
                      ;; run to skip them.
                      (dispatch-event-group (scene-initial-events scene))
                      (return-from run-one-cycle-immediate
                        (values :complete next-start t))))))
               (setf events rest)))
    (let ((result (wait-until-event-or-swap engine next-start)))
        (case result
          (:swap (values :swap nil))
          (:stopped (values :stopped nil))
          (:event
           (record-event-lateness next-start)
           (dispatch-event-group (scene-initial-events scene))
           (values :complete next-start t))))))

(defun run-one-cycle-scheduled (engine scene cycle-start skip-initial-p)
  "Run one cycle while timestamping musical events shortly ahead of time."
  (let* ((seconds-per-beat (seconds-per-beat scene))
         (ahead *event-schedule-ahead-seconds*)
         (next-start
           (+ cycle-start
              (* seconds-per-beat
                 (coerce (scene-length scene) 'double-float)))))
    (loop with events = (if skip-initial-p
                            (scene-events-after-initial scene)
                            (scene-events scene))
          while events
          do (multiple-value-bind (group deadline rest)
                 (next-event-group events scene cycle-start seconds-per-beat)
               (when (event-at-or-after-pending-swap-p engine deadline)
                 (let ((result (wait-until-pending-swap engine)))
                   (return-from run-one-cycle-scheduled
                     (values result nil))))
               (let* ((host-time (event-host-time deadline))
             (queue-time (max (monotonic-seconds)
                              (- deadline ahead)))
             (result (wait-until-event-or-swap engine queue-time)))
                 (case result
                   (:swap (return-from run-one-cycle-scheduled
                            (values :swap nil)))
                   (:stopped (return-from run-one-cycle-scheduled
                              (values :stopped nil)))
                   (:event
                    (let ((paired-note-offs
                            (dispatch-event-group-at-host-time-with-paired-note-offs
                             group rest scene cycle-start seconds-per-beat
                             host-time
                             (pending-swap-time engine))))
                      (setf rest
                            (remove-paired-note-offs rest paired-note-offs)))
                    (when (same-deadline-p deadline next-start)
                      (dispatch-event-group-at-host-time-with-paired-note-offs
                       (scene-initial-events scene)
                       (scene-events-after-initial scene)
                       scene next-start seconds-per-beat host-time)
                      (return-from run-one-cycle-scheduled
                        (values :complete next-start t))))))
               (setf events rest)))
    (let ((result (wait-until-event-or-swap engine next-start)))
      (case result
        (:swap (values :swap nil))
        (:stopped (values :stopped nil))
        (:event
         (let ((host-time (event-host-time next-start)))
           (dispatch-event-group-at-host-time-with-paired-note-offs
            (scene-initial-events scene)
            (scene-events-after-initial scene)
            scene next-start seconds-per-beat host-time))
         (values :complete next-start t))))))

(defun run-one-cycle (engine scene cycle-start skip-initial-p)
  (if (and *timestamp-midi-events*
           (midi-event-scheduling-supported-p))
      (run-one-cycle-scheduled engine scene cycle-start skip-initial-p)
      (run-one-cycle-immediate engine scene cycle-start skip-initial-p)))

(defun ensure-timestamped-events-ready ()
  (when *timestamp-midi-events*
    (unless (eq *midi-timestamped-sender-kind* :custom)
      (prepare-midi-timestamped-sender))
    (unless (midi-event-scheduling-supported-p)
      (error "Timestamped MIDI events are enabled, but CoreMIDI event scheduling is not available. Diagnostics: ~S"
             (midi-clock-diagnostics)))))

(defun ensure-mts-ready ()
  (when *mts-enabled*
    (unless (eq *midi-sysex-sender-kind* :custom)
      (prepare-midi-sysex-sender))))

(defun next-cycle-origin (scheduled-start scene now)
  "Keep the absolute musical grid unless the clock missed a whole loop.

Small dispatch/scheduling delays must not accumulate into tempo drift. After
a suspension longer than one complete loop, rebasing avoids a burst of stale
events while trying to catch up."
  (cond
    ((scene-link-enabled scene)
     (refresh-link-tempo-for-scene scene)
     (if (> (- now scheduled-start) (scene-duration-seconds scene))
         (link-scene-cycle-start-time scene now)
         scheduled-start))
    ((> (- now scheduled-start) (scene-duration-seconds scene))
     now)
    (t scheduled-start)))

(defun midi-clock-snapshot (engine)
  (with-engine-lock (engine)
    (values (engine-midi-clock-running-p engine)
            (engine-midi-clock-port engine)
            (engine-midi-clock-tempo engine)
            (engine-midi-clock-start-time engine))))

(defun midi-clock-interval (tempo)
  (/ 60d0 (* 24d0 (coerce tempo 'double-float))))

(defun midi-clock-loop (engine)
  "Dedicated absolute-time MIDI Clock sender (24 PPQN)."
  (let ((active-port nil)
        (next-tick (monotonic-seconds)))
    (unwind-protect
         (handler-case
             (loop
               (multiple-value-bind (running-p requested-port tempo start-time)
                   (midi-clock-snapshot engine)
                 (declare (ignore start-time))
                 (unless running-p (return))
                 (unless (equal active-port requested-port)
                   (when active-port
                     (send-midi-realtime active-port #xFC))
                   (setf active-port requested-port
                         next-tick (monotonic-seconds))
                   (when active-port
                     (send-midi-realtime active-port #xFA)))
                 (if (null active-port)
                     (sleep 0.02d0)
                     (let* ((interval (midi-clock-interval tempo))
                            (now (monotonic-seconds)))
                       (cond
                         ((>= now next-tick)
                          (send-midi-realtime active-port #xF8)
                          ;; MIDI receivers infer BPM from the interval
                          ;; between successive F8 bytes. Scheduling from the
                          ;; old absolute deadline after a late tick makes the
                          ;; next interval too short: a subtle catch-up burst.
                          ;; Anchor the next deadline to the actual send time
                          ;; instead. This forbids compressed intervals even
                          ;; after GC or a temporarily delayed Lisp process.
                          (setf next-tick
                                (+ (monotonic-seconds) interval)))
                         (t
                          (precise-sleep-until next-tick)))))))
           (error (condition)
             (setf (engine-last-error engine) condition
                   (engine-midi-clock-running-p engine) nil)
             (format *error-output*
                     "~&Livecode MIDI Clock stopped: ~A~%" condition)))
      (when active-port
        (ignore-errors (send-midi-realtime active-port #xFC))))))

(defun scheduled-midi-clock-loop (engine)
  "MIDI Clock sender that queues timestamped CoreMIDI packets ahead of time."
  (let ((active-port nil)
        (scheduled-host-time nil)
        (ahead-seconds 0.50d0)
        (chunk-seconds 0.10d0))
    (unwind-protect
         (handler-case
             (loop
               (multiple-value-bind (running-p requested-port tempo start-time)
                   (midi-clock-snapshot engine)
                 (unless running-p (return))
                 (unless (equal active-port requested-port)
                   (when active-port
                     (send-midi-realtime active-port #xFC))
                   (setf active-port requested-port)
                   (when active-port
                     (let* ((interval-host-ticks
                              (core-midi-seconds-to-host-ticks
                               (midi-clock-interval tempo)))
                            (musical-start-delay
                              (max 0.01d0
                                   (- start-time
                                      (monotonic-seconds))))
                            (host-musical-start
                              (+ (core-midi-host-time)
                                 (core-midi-seconds-to-host-ticks
                                  musical-start-delay)))
                            (host-start
                              (- host-musical-start
                                 interval-host-ticks)))
                       (send-midi-realtime-at-host-time
                        active-port #xFA host-start)
                       ;; Musical beat 0 is aligned with the first F8 after
                       ;; START. This lets clock-driven arpeggiators and
                       ;; Livecode's own notes share a single phase origin.
                       (setf scheduled-host-time
                             host-musical-start))))
                 (if (null active-port)
                     (sleep 0.02d0)
                     (let* ((host-now (core-midi-host-time))
                            (interval-host-ticks
                              (core-midi-seconds-to-host-ticks
                               (midi-clock-interval tempo)))
                            (target-host-time
                              (+ host-now
                                 (core-midi-seconds-to-host-ticks
                                  ahead-seconds))))
                       ;; Maintain the MIDI clock grid entirely in CoreMIDI
                       ;; host ticks. The Lisp thread only keeps the queue
                       ;; filled; it no longer participates in tick spacing.
                       (loop while (< scheduled-host-time target-host-time)
                             do (send-midi-realtime-at-host-time
                                 active-port #xF8 scheduled-host-time)
                                (incf scheduled-host-time
                                      interval-host-ticks))
                       (sleep chunk-seconds)))))
           (error (condition)
             (setf (engine-last-error engine) condition
                   (engine-midi-clock-running-p engine) nil)
             (format *error-output*
                     "~&Livecode scheduled MIDI Clock stopped: ~A~%"
                     condition))))
      (when active-port
        (ignore-errors (send-midi-realtime active-port #xFC)))))

(defun start-midi-clock (engine port tempo)
  (let ((start-time
          (+ (monotonic-seconds)
             *midi-clock-start-delay-seconds*
             (midi-clock-interval tempo))))
    (with-engine-lock (engine)
      (setf (engine-midi-clock-port engine) port
            (engine-midi-clock-tempo engine) tempo
            ;; Musical cycle starts on the first F8 after START. START itself
            ;; is sent one MIDI-clock interval earlier by the clock thread.
            (engine-midi-clock-start-time engine) start-time
            (engine-midi-clock-running-p engine) (not (null port)))))
  (when (and port (not (midi-realtime-scheduling-supported-p)))
    (error "Timestamped CoreMIDI MIDI Clock is not available: ~S"
           (midi-clock-diagnostics)))
  (when (and port
             (not (thread-alive-p (engine-midi-clock-thread engine))))
    (setf (engine-midi-clock-thread engine)
          (spawn-engine-thread (lambda ()
                                 (scheduled-midi-clock-loop engine))
                               "Livecode MIDI clock")))
  port)

(defun configure-midi-clock (engine scene)
  (let ((port (scene-midi-clock-port scene)))
    (if port
        (start-midi-clock engine port (scene-tempo scene))
        (with-engine-lock (engine)
          (setf (engine-midi-clock-port engine) nil
                (engine-midi-clock-running-p engine) nil)))))

(defun initial-cycle-start-time (scene)
  (if (scene-link-enabled scene)
      (progn
        (configure-link-for-scene scene)
        (link-scene-cycle-start-time scene
                                    (timestamped-safe-swap-request-time
                                     (monotonic-seconds))))
      (monotonic-seconds)))

(defun stop-midi-clock (engine)
  (when engine
    (with-engine-lock (engine)
      (setf (engine-midi-clock-running-p engine) nil
            (engine-midi-clock-port engine) nil)))
  :stopped)

(defun clock-loop (engine)
  (setf (engine-cycle-start-time engine)
        (initial-cycle-start-time (engine-current-scene engine)))
  (configure-midi-clock engine (engine-current-scene engine))
  (when (and (scene-midi-clock-port (engine-current-scene engine))
             (not (scene-link-enabled (engine-current-scene engine))))
    (setf (engine-cycle-start-time engine)
          (engine-midi-clock-start-time engine)))
  (unwind-protect
       (handler-case
           (loop while (engine-live-p engine)
                 for scene = (engine-current-scene engine)
                 do (refresh-link-tempo-for-scene scene)
                    (multiple-value-bind (result next-start skip-next-initial-p)
                        (run-one-cycle engine scene
                                       (engine-cycle-start-time engine)
                                       (engine-skip-initial-events-p engine))
                      (case result
                        (:complete
                         (incf (engine-cycle-number engine))
                         (setf (engine-cycle-start-time engine)
                               (next-cycle-origin next-start scene
                                                  (monotonic-seconds))
                               (engine-skip-initial-events-p engine)
                               skip-next-initial-p))
                        (:swap
                         (multiple-value-bind (new-scene start-time)
                             (take-pending-scene engine)
                           (when new-scene
                             (release-active-notes)
                             (configure-link-for-scene new-scene)
                             (configure-midi-clock engine new-scene)
                             (setf (engine-current-scene engine) new-scene
                                   (engine-cycle-start-time engine) start-time
                                   (engine-skip-initial-events-p engine) nil))))
                        (:stopped (return)))))
         (error (condition)
           (setf (engine-last-error engine) condition)
           (format *error-output* "~&Livecode clock stopped: ~A~%" condition)))
    (setf (engine-running-p engine) nil)
    (stop-midi-clock engine)
    (ignore-errors (panic))))

(defun start-engine (scene &key (beats-per-bar *beats-per-bar*))
  ;; START-ENGINE runs on the user's evaluation process. Capture the dynamic
  ;; Opusmodus MIDI destination before either Livecode process is spawned.
  (when (and (scene-midi-clock-port scene)
             (not (eq *midi-realtime-sender-kind* :custom)))
    (prepare-midi-realtime-sender))
  (ensure-timestamped-events-ready)
  (ensure-mts-ready)
  (when (scene-link-enabled scene)
    (configure-link-for-scene scene)
    (start-link-transport-for-scene scene))
  (reset-event-lateness-stats)
  (let ((engine (make-engine :lock (make-engine-lock)
                             :running-p t
                             :current-scene scene
                             :beats-per-bar beats-per-bar)))
    (setf (engine-thread engine)
          (spawn-engine-thread (lambda () (clock-loop engine))
                               "Livecode scheduler"))
    (setf *engine* engine)
    (pushnew engine *livecode-engines* :test #'eq)
    engine))

(defun submit-scene (scene &key (quantize *swap-quantum*)
                                (beats-per-bar *beats-per-bar*))
  ;; A running scene may enable or redirect MIDI Clock. Refresh the raw sender
  ;; while we are still on the evaluation process, not in the scheduler.
  (when (and (scene-midi-clock-port scene)
             (not (eq *midi-realtime-sender-kind* :custom)))
    (prepare-midi-realtime-sender))
  (ensure-timestamped-events-ready)
  (ensure-mts-ready)
  (when (scene-link-enabled scene)
    (configure-link-for-scene scene))
  (if (and *engine* (engine-live-p *engine*))
      (progn
        (with-engine-lock (*engine*)
          (setf (engine-beats-per-bar *engine*) beats-per-bar
                (engine-pending-scene *engine*) scene
                (engine-pending-quantization *engine*) quantize
                (engine-pending-start-time *engine*)
                (next-live-swap-time *engine* quantize
                                     (monotonic-seconds))))
        :queued)
      (progn
        (start-engine scene :beats-per-bar beats-per-bar)
        :started)))

(defun stop-live ()
  ;; Stop every engine known to this image, not only the most recently
  ;; assigned one. This also protects against accidental duplicate clocks.
  (dolist (engine (copy-list *livecode-engines*))
    (ignore-errors
      (when (engine-current-scene engine)
        (stop-link-transport-for-scene (engine-current-scene engine))))
    (ignore-errors (setf (engine-running-p engine) nil))
    (ignore-errors (stop-midi-clock engine)))
  (when *engine*
    (ignore-errors
      (when (engine-current-scene *engine*)
        (stop-link-transport-for-scene (engine-current-scene *engine*))))
    (ignore-errors (setf (engine-running-p *engine*) nil))
    (ignore-errors (stop-midi-clock *engine*)))
  (sleep 0.03d0)
  (setf *livecode-engines*
        (remove-if-not #'engine-live-p *livecode-engines*))
  (unless (engine-live-p *engine*)
    (setf *engine* nil))
  (ignore-errors (panic))
  :stopped)

(defun condition-description (condition)
  (when condition
    (with-output-to-string (stream)
      (format stream "~A" condition))))

(defun live-last-error ()
  "Return and print the complete error that stopped the Livecode clock."
  (let ((condition (engine-last-error-safe *engine*)))
    (if condition
        (let ((description (condition-description condition)))
          (format t "~&Livecode error (~A): ~A~%"
                  (type-of condition) description)
          description)
        (progn
          (format t "~&Livecode: no recorded error.~%")
          nil))))

(defun live-status ()
  (let ((condition (engine-last-error-safe *engine*)))
    (if (and *engine* (engine-live-p *engine*))
        (handler-case
            (list :running t
                  :cycle (engine-cycle-number *engine*)
                  :tempo (scene-tempo (engine-current-scene *engine*))
                  :link-enabled
                  (scene-link-enabled (engine-current-scene *engine*))
                  :link-status
                  (and (scene-link-enabled (engine-current-scene *engine*))
                       (ignore-errors
                        (ableton-link-status
                         :quantum
                         (scene-link-quantum
                          (engine-current-scene *engine*)))))
                  :beat-seconds
                  (seconds-per-beat (engine-current-scene *engine*))
                  :loop-beats
                  (scene-length (engine-current-scene *engine*))
                  :loop-seconds
                  (scene-duration-seconds (engine-current-scene *engine*))
                  :midi-clock-running
                  (engine-midi-clock-running-p *engine*)
                  :midi-clock-sender
                  *midi-realtime-sender-kind*
                  :midi-clock-scheduled
                  (midi-realtime-scheduling-supported-p)
                  :midi-events-sender
                  *midi-timestamped-sender-kind*
                  :midi-events-scheduled
                  (and *timestamp-midi-events*
                       (midi-event-scheduling-supported-p))
                  :midi-events-timestamping-enabled
                  *timestamp-midi-events*
                  :mts-enabled
                  *mts-enabled*
                  :mts-device-id
                  *mts-device-id*
                  :mts-tuning-program
                  *mts-tuning-program*
                  :mts-lead-time
                  *mts-lead-time-seconds*
                  :sound-set-articulations-enabled
                  *sound-set-articulations-enabled*
                  :keyswitch-lead-time
                  *keyswitch-lead-time-seconds*
                  :keyswitch-duration
                  *keyswitch-duration-seconds*
                  :keyswitch-velocity
                  *keyswitch-velocity*
                  :sound-set-registry
                  (ignore-errors
                   (list :directory
                         (and *sound-set-registry-directory*
                              (namestring *sound-set-registry-directory*))
                         :auto-reload *sound-set-auto-reload*
                         :files *sound-set-registry-files*
                         :soundsets (hash-table-count *sound-set-registry*)))
                  :midi-events-ahead
                  *event-schedule-ahead-seconds*
                  :midi-events-swap-safety-margin
                  *timestamped-swap-safety-margin-seconds*
                  :midi-events-wakeup-resolution
                  *event-wakeup-resolution-seconds*
                  :midi-events-late-count
                  *event-late-count*
                  :midi-events-max-lateness
                  *event-max-lateness-seconds*
                  :midi-events-lateness-threshold
                  *event-lateness-warning-seconds*
                  :midi-clock-port
                  (engine-midi-clock-port *engine*)
                  :midi-clock-output-binding
                  (captured-midi-output-port)
                  :midi-clock-tempo
                  (and (engine-midi-clock-running-p *engine*)
                       (engine-midi-clock-tempo *engine*))
                  :midi-clock-event-offset
                  *midi-clock-event-offset-seconds*
                  :known-engines (length *livecode-engines*)
                  :pending (not (null (engine-pending-scene *engine*)))
                  :pending-quantization
                  (and (engine-pending-scene *engine*)
                       (engine-pending-quantization *engine*))
                  :pending-in-seconds
                  (and (engine-pending-start-time *engine*)
                       (max 0d0
                            (- (engine-pending-start-time *engine*)
                               (monotonic-seconds))))
                  :pending-tempo
                  (and (engine-pending-scene *engine*)
                       (scene-tempo (engine-pending-scene *engine*)))
                  :last-error (condition-description condition))
          (error (status-error)
            (setf *engine* nil)
            (list :running nil
                  :status-error (condition-description status-error)
                  :last-error (condition-description condition))))
        (list :running nil
              :last-error-type (and condition (type-of condition))
              :last-error (condition-description condition)))))
