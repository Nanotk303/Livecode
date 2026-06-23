(in-package #:livecode)

(defparameter *length-values*
  '((w . 4) (h . 2) (q . 1) (e . 1/2) (s . 1/4)
    (t . 1/8) (x . 1/16)))

(defparameter *velocity-values*
  '((ppppp . 8) (pppp . 16) (ppp . 24) (pp . 36) (p . 48)
    (mp . 60) (mf . 76) (f . 92) (ff . 108) (fff . 120)
    (ffff . 124) (fffff . 127)))

(defparameter *pitch-classes*
  '(("C" . 0) ("CS" . 1) ("DF" . 1) ("DB" . 1) ("D" . 2)
    ("DS" . 3) ("EF" . 3) ("EB" . 3) ("E" . 4) ("F" . 5)
    ("FS" . 6) ("GF" . 6) ("GB" . 6) ("G" . 7) ("GS" . 8)
    ("AF" . 8) ("AB" . 8) ("A" . 9) ("AS" . 10)
    ("BF" . 10) ("BB" . 10)
    ("B" . 11)))

(defun opusmodus-function (name)
  (labels ((find-in-package (package)
             (when package
               (multiple-value-bind (symbol status)
                   (find-symbol name package)
                 (when (and status (fboundp symbol))
                   (values (symbol-function symbol) symbol))))))
    (dolist (package-name '("OPUSMODUS" "OM" "CL-USER"))
      (multiple-value-bind (function symbol)
          (find-in-package (find-package package-name))
        (when function
          (return-from opusmodus-function (values function symbol)))))
    ;; Some useful Opusmodus implementation functions live in private
    ;; packages. Looking up an exact function name is safer than guessing its
    ;; package from one release to another.
    (dolist (package (list-all-packages))
      (multiple-value-bind (function symbol) (find-in-package package)
        (when function
          (return-from opusmodus-function (values function symbol)))))
    (values nil nil)))

(defun proper-list-copy (value)
  (if (listp value) (copy-tree value) value))

(defun opusmodus-length-to-beats (length)
  "Convert an Opusmodus whole-note fraction to quarter-note beats."
  (* 4 length))

(defun unquote (value)
  (if (and (consp value)
           (eq (first value) 'quote)
           (null (cddr value)))
      (second value)
      value))

(defun resolve-source (source)
  (let ((source (unquote source)))
    (if (and (symbolp source) (boundp source))
        (symbol-value source)
        source)))

(defun named-assoc-value (symbol alist)
  (when (symbolp symbol)
    (loop for (key . value) in alist
          when (string-equal (symbol-name symbol) (symbol-name key))
            return value)))

(defun velocity-symbol-value (symbol)
  (when (symbolp symbol)
    (or (named-assoc-value symbol *velocity-values*)
        (let* ((name (symbol-name symbol))
               (trimmed (string-trim "<>" name)))
          (when (< 0 (length trimmed) (length name))
            (named-assoc-value
             (intern (string-upcase trimmed) (symbol-package symbol))
             *velocity-values*))))))

(defun dynamic-value (value default)
  (cond ((numberp value) value)
        ((velocity-symbol-value value))
        (t default)))

(defun parse-length-symbol (symbol)
  (let* ((name (string-downcase (symbol-name symbol)))
         (rest-p (and (plusp (length name))
                      (char= (char name 0) #\-)))
         (plain (if rest-p (subseq name 1) name))
         (dots (loop for char across plain count (char= char #\.)))
         (base-name (string-right-trim "." plain))
         (base-symbol (find-symbol (string-upcase base-name) :livecode))
         (base (cdr (assoc base-symbol *length-values* :test #'eq))))
    (when base
      (values (* base
                 (loop with factor = 1
                       with addition = 1/2
                       repeat dots
                       do (incf factor addition)
                          (setf addition (/ addition 2))
                       finally (return factor)))
              rest-p))))

(defun parse-pitch-symbol (symbol)
  (let* ((name (string-upcase (symbol-name symbol)))
         (size (length name)))
    (when (>= size 2)
      (let* ((octave-char (char name (1- size)))
             (pitch-name (subseq name 0 (1- size)))
             (pitch-class (cdr (assoc pitch-name *pitch-classes*
                                      :test #'string=))))
        (when (and pitch-class (digit-char-p octave-char))
          (+ (* 12 (1+ (digit-char-p octave-char))) pitch-class))))))

(defun alexandria-free-flatten (tree)
  (labels ((walk (node)
             (cond ((null node) nil)
                   ((atom node) (list node))
                   (t (mapcan #'walk node)))))
    (walk tree)))

(defun flatten-articulation-events (tree &optional expected-events)
  "Flatten Opusmodus articulation structure while preserving NIL placeholders.

The generic flatten helper intentionally drops NIL because it is useful for
pitch/velocity streams.  Articulations are different: NIL can mean \"no
explicit articulation for this note\" and must stay aligned with lengths."
  (labels ((walk (node)
             (if (consp node)
                 (mapcan #'walk node)
                 (list node))))
    (let ((values (and tree (walk tree))))
      (cond
        ((and expected-events values (< (length values) expected-events))
         (append values
                 (make-list (- expected-events (length values))
                            :initial-element nil)))
        ((and expected-events values (> (length values) expected-events))
         (subseq values 0 expected-events))
        (t values)))))

(defun articulation-placeholder-p (value)
  "True when VALUE means no explicit articulation in an OMN articulation stream."
  (or (null value)
      (and (symbolp value)
           (string= (symbol-name value) "-"))))

(defun source-articulation-events (source &optional expected-events)
  "Infer articulation events directly from simple OMN source syntax.

This complements Opusmodus' DISASSEMBLE-OMN output.  Some Opusmodus versions
return '-' for continued articulations such as LEG; keeping a direct pass over
the original OMN lets Livecode recover the articulation the performer wrote."
  (let ((current-articulation nil)
        (pending-event nil)
        (pending-has-pitch-p nil)
        articulations)
    (labels ((emit-pending ()
               (when pending-event
                 (push current-articulation articulations)
                 (setf pending-event nil))))
      (dolist (item (alexandria-free-flatten source))
        (when (symbolp item)
          (multiple-value-bind (length rest-p) (parse-length-symbol item)
            (declare (ignore rest-p))
            (let ((pitch (parse-pitch-symbol item))
                  (velocity (velocity-symbol-value item)))
              (cond
                (length
                 (emit-pending)
                 (setf pending-event t
                       pending-has-pitch-p nil))
                (pitch
                 (when (and pending-event pending-has-pitch-p)
                   (emit-pending))
                 (setf pending-event t
                       pending-has-pitch-p t))
                (velocity)
                ((member item '(< > <> ><) :test #'string-equal))
                (t
                 (setf current-articulation item)))))))
      (emit-pending))
    (flatten-articulation-events (nreverse articulations) expected-events)))

(defun merge-articulation-events (primary fallback)
  "Use FALLBACK only where PRIMARY has NIL or '-' placeholders."
  (loop for primary-value in primary
        for fallback-values = fallback then (rest fallback-values)
        for fallback-value = (first fallback-values)
        collect (if (and (articulation-placeholder-p primary-value)
                         (not (articulation-placeholder-p fallback-value)))
                    fallback-value
                    primary-value)))

(defun flatten-pitch-events (tree &optional expected-events)
  "Flatten phrase nesting while preserving nested numeric chord lists.

The outer list is always an event sequence. It must therefore never be
interpreted as one chord merely because all of its pitches are numeric.
EXPECTED-EVENTS resolves Opusmodus' ambiguous ((60 62 ...)) representation:
with several corresponding lengths it is a phrase, while with one length it
is a chord."
  (labels ((walk-event (node)
             (cond
               ((null node) nil)
               ((numberp node) (list node))
               ((and (listp node) (every #'numberp node))
                (list node))
               ((listp node) (mapcan #'walk-event node))
               (t (list node)))))
    (let ((events (if (listp tree)
                      (mapcan #'walk-event tree)
                      (walk-event tree))))
      ;; DISASSEMBLE-OMN commonly wraps a generated monophonic phrase in one
      ;; extra list. If treating that numeric list as a chord produces one
      ;; event but the duration stream proves that every scalar is a distinct
      ;; event, flatten it. Genuine chords remain grouped because their scalar
      ;; pitch count is greater than the corresponding event count.
      (if (and expected-events
               (/= (length events) expected-events)
               (= (length (alexandria-free-flatten events))
                  expected-events))
          (alexandria-free-flatten events)
          events))))

(defun normalize-opusmodus-components (source)
  (let ((disassemble (opusmodus-function "DISASSEMBLE-OMN"))
        (pitch-to-midi (opusmodus-function "PITCH-TO-MIDI")))
    (when (and disassemble pitch-to-midi)
      (let* ((parts (funcall disassemble source))
             ;; DISASSEMBLE-OMN preserves bars/phrases as nested lists. The
             ;; V1 scheduler is linear, so flatten those phrase boundaries
             ;; before pairing lengths, pitches and velocities.
             ;; Opusmodus represents OMN lengths as fractions of a whole
             ;; note (q = 1/4). Livecode's scheduler uses quarter-note beats,
             ;; so convert them here (q = 1 beat, h = 2 beats, etc.).
             (lengths
               (mapcar #'opusmodus-length-to-beats
                       (alexandria-free-flatten
                        (getf parts :length))))
             (pitches
               (flatten-pitch-events
                (funcall pitch-to-midi (getf parts :pitch))
                (count-if #'plusp lengths)))
             (velocities (alexandria-free-flatten
                          (getf parts :velocity)))
             (note-count (count-if #'plusp lengths))
             (articulations
               (merge-articulation-events
                (flatten-articulation-events
                 (getf parts :articulation)
                 note-count)
                (source-articulation-events source note-count))))
        (list :length lengths
              :pitch pitches
              :velocity velocities
              :articulation articulations)))))

(defun fallback-omn-components (source)
  "Parse a deliberately small but useful linear OMN subset.
Supported: w/h/q/e/s/t/x, dotted lengths, rests, pitches C0..B9,
accidentals s/f, and dynamic symbols."
  (let ((current-length 1)
        (current-pitch 60)
        (current-velocity 76)
        (current-articulation nil)
        lengths pitches velocities
        articulations
        (pending-event nil))
    (labels ((emit-pending ()
               (when pending-event
                 (push current-length lengths)
                 (unless (minusp current-length)
                   (push current-pitch pitches)
                   (push current-velocity velocities)
                   (push current-articulation articulations))
                 (setf pending-event nil))))
      (let ((pending-has-pitch-p nil))
      (dolist (item (alexandria-free-flatten source))
        (cond
          ((numberp item)
           (emit-pending)
           (setf current-length item
                 pending-event t
                 pending-has-pitch-p nil))
          ((symbolp item)
           (multiple-value-bind (length rest-p) (parse-length-symbol item)
             (let ((pitch (parse-pitch-symbol item))
                   (velocity (velocity-symbol-value item)))
               (cond
                 (length
                  (emit-pending)
                  (setf current-length (if rest-p (- length) length)
                        pending-event t
                        pending-has-pitch-p nil))
                 (pitch
                  (when (and pending-event pending-has-pitch-p)
                    (emit-pending))
                  (setf current-pitch pitch
                        pending-event t
                        pending-has-pitch-p t))
                 (velocity
                  (setf current-velocity velocity))
                 ((member item '(< > <> ><) :test #'string-equal))
                 (t
                  (setf current-articulation item))))))))
      (emit-pending))
    (list :length (nreverse lengths)
          :pitch (nreverse pitches)
          :velocity (nreverse velocities)
          :articulation (nreverse articulations)))))

(defun components-to-notes (components &key include-articulation)
  (let ((pitches (copy-list (getf components :pitch)))
        (velocities (copy-list (getf components :velocity)))
        (articulations (copy-list (getf components :articulation)))
        (last-pitch 60)
        (last-velocity 76)
        (beat 0)
        notes)
    (dolist (length (getf components :length))
      (let ((duration (abs length)))
        (unless (minusp length)
          (let ((articulation
                  (and articulations
                       (pop articulations))))
          (when pitches (setf last-pitch (pop pitches)))
          (when velocities (setf last-velocity
                                 (dynamic-value (pop velocities)
                                                last-velocity)))
          (push (if include-articulation
                    (list beat duration last-pitch last-velocity
                          articulation)
                    (list beat duration last-pitch last-velocity))
                notes)))
        (incf beat duration)))
    (values (nreverse notes) beat)))

(defun compile-omn-components (source)
  (let* ((source (proper-list-copy (resolve-source source)))
         (native (normalize-opusmodus-components source))
         (components (or native (fallback-omn-components source))))
    components))

(defun compile-omn-for-track (source)
  "Return notes with articulation data for the live engine.

Each note is (beat duration midi velocity articulation)."
  (components-to-notes (compile-omn-components source)
                       :include-articulation t))

(defun compile-omn (source)
  "Return (values notes length). Each note is (beat duration midi velocity)."
  (components-to-notes (compile-omn-components source)))
