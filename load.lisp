(defun %livecode-reset-reload-state ()
  "Forget stale Livecode runtime objects without touching their accessors.

This is deliberately symbol-based: after a DEFSTRUCT ENGINE change, LispWorks
can keep old ENGINE instances around, and calling ENGINE-* accessors on them
signals \"obsolete structure\" errors during reload.
"
  (let ((package (find-package :livecode)))
    (when package
      (dolist (name '("*ENGINE*"
                      "*LIVECODE-ENGINES*"
                      "*MIDI-SENDER*"
                      "*MIDI-REALTIME-SENDER*"
                      "*MIDI-REALTIME-SENDER-KIND*"
                      "*MIDI-TIMESTAMPED-SENDER*"
                      "*MIDI-TIMESTAMPED-SENDER-KIND*"
                      "*MIDI-SYSEX-SENDER*"
                      "*MIDI-SYSEX-SENDER-KIND*"))
        (multiple-value-bind (symbol status) (find-symbol name package)
          (when (and status (boundp symbol))
            (ignore-errors (setf (symbol-value symbol) nil)))))
      (multiple-value-bind (symbol status) (find-symbol "*ACTIVE-NOTES*" package)
        (when (and status (boundp symbol))
          (ignore-errors
            (setf (symbol-value symbol) (make-hash-table :test #'equal))))))))

(let* ((this-file (or *load-truename* *compile-file-truename*))
       (root (make-pathname :name nil :type nil :defaults this-file)))
  ;; Reloading while performing used to orphan the previous clock threads:
  ;; MODEL.LISP reset *ENGINE* before they could be stopped. Shut the running
  ;; engine down before redefining anything.
  (let* ((package (find-package :livecode))
         (stop-symbol (and package (find-symbol "STOP-LIVE" package))))
    (when (and stop-symbol (fboundp stop-symbol))
      (ignore-errors (funcall stop-symbol))))
  ;; Clean up orphaned processes left by older Livecode versions that reset
  ;; *ENGINE* during reload and therefore lost their thread handles.
  #+lispworks
  (dolist (process (mp:list-all-processes))
    (let ((name (ignore-errors (mp:process-name process))))
      (when (and (stringp name)
                 (search "Livecode" name :test #'char-equal)
                 (not (eq process mp:*current-process*)))
        (ignore-errors (mp:process-kill process)))))
  (%livecode-reset-reload-state)
  (load (merge-pathnames "src/package.lisp" root))
  (load (merge-pathnames "src/model.lisp" root))
  (load (merge-pathnames "src/platform.lisp" root))
  (load (merge-pathnames "src/link.lisp" root))
  (load (merge-pathnames "src/omn.lisp" root))
  (load (merge-pathnames "src/soundsets.lisp" root))
  (load (merge-pathnames "src/midi.lisp" root))
  (load (merge-pathnames "src/engine.lisp" root))
  (load (merge-pathnames "src/api.lisp" root))
  (%livecode-reset-reload-state))
