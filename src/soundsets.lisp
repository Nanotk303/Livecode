(in-package #:livecode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require :asdf)))

(defstruct sound-set-definition
  name
  source-file
  supports-mts
  (programs (make-hash-table :test #'equalp))
  (controllers (make-hash-table :test #'equalp)))

(defun directory-pathname-designator (value)
  (etypecase value
    (pathname (uiop:ensure-directory-pathname value))
    (string
     (uiop:ensure-directory-pathname
      (uiop:parse-native-namestring value)))))

(defun environment-directory (name)
  (let ((value (ignore-errors (uiop:getenv name))))
    (and value
         (plusp (length value))
         (directory-pathname-designator value))))

(defun package-symbol-value (package-name symbol-name)
  (let ((package (find-package package-name)))
    (when package
      (multiple-value-bind (symbol status)
          (find-symbol symbol-name package)
        (when (and status (boundp symbol))
          (symbol-value symbol))))))

(defun opusmodus-sound-set-directory-candidates ()
  "Return likely Opusmodus soundset roots for different user installations."
  (let* ((home (user-homedir-pathname))
         (opusmodus-root
           (or (package-symbol-value "OPUSMODUS" "*OPUSMODUS-DIRECTORY*")
               (package-symbol-value "OPUSMODUS" "OPUSMODUS-DIRECTORY")
               (package-symbol-value "OPUSMODUS" "*OPUSMODUS-HOME*")))
         (opusmodus-user-source
           (or (package-symbol-value "OPUSMODUS" "*USER-SOURCE*")
               (package-symbol-value "OPUSMODUS" "*OPUSMODUS-USER-SOURCE*"))))
    (remove nil
            (list
             (environment-directory "LIVECODE_SOUND_SET_DIRECTORY")
             (environment-directory "OPUSMODUS_SOUND_SET_DIRECTORY")
             (merge-pathnames
              #P"Opusmodus/User Source/Libraries/Def-Sound-Sets/"
              home)
             (merge-pathnames
              #P"Documents/Opusmodus/User Source/Libraries/Def-Sound-Sets/"
              home)
             (and opusmodus-root
                  (merge-pathnames
                   #P"User Source/Libraries/Def-Sound-Sets/"
                   (directory-pathname-designator opusmodus-root)))
             (and opusmodus-user-source
                  (merge-pathnames
                   #P"Libraries/Def-Sound-Sets/"
                   (directory-pathname-designator opusmodus-user-source)))))))

(defun default-sound-set-directory ()
  (or (find-if #'probe-file (opusmodus-sound-set-directory-candidates))
      (first (opusmodus-sound-set-directory-candidates))))

(defparameter *sound-set-directory*
  (default-sound-set-directory)
  "Root directory scanned for Opusmodus DEF-SOUND-SET files.")

(defparameter *sound-set-auto-reload* t
  "When true, Livecode rescans saved soundset files when their fingerprint changes.")

(defvar *sound-set-registry* (make-hash-table :test #'equalp))
(defvar *sound-set-registry-directory* nil)
(defvar *sound-set-registry-fingerprint* nil)
(defvar *sound-set-registry-files* 0)
(defvar *sound-set-registry-errors* nil)

(defun canonical-sound-set-name (name)
  (string-upcase
   (etypecase (unquote name)
     (symbol (symbol-name (unquote name)))
     (string (unquote name)))))

(defun sound-set-keyword-p (value keyword-name)
  (and (symbolp value)
       (string-equal (symbol-name value) keyword-name)))

(defun sound-set-plist-value (plist keyword-name)
  (loop for (key value) on plist by #'cddr
        when (sound-set-keyword-p key keyword-name)
          return value))

(defun sound-set-lisp-files (directory)
  (labels ((walk (dir)
             (append (uiop:directory-files dir "*.lisp")
                     (mapcan #'walk (uiop:subdirectories dir)))))
    (when (probe-file directory)
      (walk directory))))

(defun sound-set-files-fingerprint (files)
  (list :count (length files)
        :latest-write-date
        (loop for file in files
              maximize (or (ignore-errors (file-write-date file)) 0))))

(defun parse-sound-set-pairs-into (pairs table)
  "Parse an Opusmodus :PROGRAMS or :CONTROLLERS soundset list into TABLE."
  (loop with rest = pairs
        while rest
        for key = (first rest)
        do (cond
             ((sound-set-keyword-p key "GROUP")
              (setf rest (cddr rest)))
             ((and (symbolp key)
                   (rest rest))
              (setf (gethash (canonical-sound-set-name key) table)
                    (second rest)
                    rest (cddr rest)))
             (t
              (setf rest (rest rest)))))
  table)

(defun register-sound-set-form (form source-file)
  (when (and (consp form)
             (symbolp (first form))
             (string-equal (symbol-name (first form)) "DEF-SOUND-SET")
             (second form))
    (let* ((name (second form))
           (body (cddr form))
           (definition
             (make-sound-set-definition
              :name name
              :source-file source-file
              :supports-mts (sound-set-plist-value body "SUPPORTS-MTS"))))
      (parse-sound-set-pairs-into
       (or (sound-set-plist-value body "PROGRAMS") nil)
       (sound-set-definition-programs definition))
      (parse-sound-set-pairs-into
       (or (sound-set-plist-value body "CONTROLLERS") nil)
       (sound-set-definition-controllers definition))
      (setf (gethash (canonical-sound-set-name name) *sound-set-registry*)
            definition)
      definition)))

(defun read-sound-set-file (file)
  (with-open-file (stream file :direction :input)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          when (register-sound-set-form form file)
            collect it)))

(defun reload-livecode-soundsets (&optional (directory *sound-set-directory*))
  "Reload Opusmodus DEF-SOUND-SET definitions from DIRECTORY recursively."
  (let* ((directory (directory-pathname-designator directory))
         (true-directory (probe-file directory)))
    (unless true-directory
      (clrhash *sound-set-registry*)
      (setf *sound-set-directory* directory
            *sound-set-registry-directory* nil
            *sound-set-registry-files* 0
            *sound-set-registry-errors*
            (list (list :directory (namestring directory)
                        :error "Soundset directory was not found."))
            *sound-set-registry-fingerprint*
            (sound-set-files-fingerprint nil))
      (return-from reload-livecode-soundsets
        (list :directory (namestring directory)
              :files 0
              :soundsets 0
              :loaded-definitions 0
              :errors 1)))
    (let* ((directory (truename true-directory))
         (files (sound-set-lisp-files directory))
         (loaded 0)
         (errors nil))
    (clrhash *sound-set-registry*)
    (dolist (file files)
      (handler-case
          (incf loaded (length (read-sound-set-file file)))
        (error (condition)
          (push (list :file (namestring file)
                      :error (princ-to-string condition))
                errors))))
    (setf *sound-set-directory* directory
          *sound-set-registry-directory* directory
          *sound-set-registry-files* (length files)
          *sound-set-registry-errors* (nreverse errors)
          *sound-set-registry-fingerprint*
          (sound-set-files-fingerprint files))
    (list :directory (namestring directory)
          :files (length files)
          :soundsets (hash-table-count *sound-set-registry*)
          :loaded-definitions loaded
          :errors (length errors)))))

(defun set-livecode-soundset-directory (directory &key (reload t))
  "Set the Opusmodus soundset root used by Livecode.

DIRECTORY may be a pathname or a native namestring.  When RELOAD is true,
the registry is immediately rescanned."
  (setf *sound-set-directory* (directory-pathname-designator directory))
  (if reload
      (reload-livecode-soundsets *sound-set-directory*)
      *sound-set-directory*))

(defun maybe-reload-livecode-soundsets ()
  (when *sound-set-auto-reload*
    (let* ((directory *sound-set-directory*)
           (files (sound-set-lisp-files directory))
           (fingerprint (sound-set-files-fingerprint files)))
      (when (or (zerop (hash-table-count *sound-set-registry*))
                (not (equal fingerprint *sound-set-registry-fingerprint*)))
        (reload-livecode-soundsets directory)))))

(defun live-soundset-status ()
  "Return the current Livecode soundset registry status."
  (maybe-reload-livecode-soundsets)
  (list :directory (and *sound-set-registry-directory*
                        (namestring *sound-set-registry-directory*))
        :auto-reload *sound-set-auto-reload*
        :files *sound-set-registry-files*
        :soundsets (hash-table-count *sound-set-registry*)
        :fingerprint *sound-set-registry-fingerprint*
        :errors *sound-set-registry-errors*))

(defun registry-sound-set (sound)
  (maybe-reload-livecode-soundsets)
  (gethash (canonical-sound-set-name sound) *sound-set-registry*))

(defun registry-sound-set-program (sound program)
  (let ((definition (registry-sound-set sound)))
    (and definition
         (gethash (canonical-sound-set-name program)
                  (sound-set-definition-programs definition)))))

(defun registry-sound-set-controller (sound controller)
  (let ((definition (registry-sound-set sound)))
    (and definition
         (gethash (canonical-sound-set-name controller)
                  (sound-set-definition-controllers definition)))))

(defun live-soundset-program (sound program)
  "Return the raw program/articulation definition Livecode will use."
  (or (registry-sound-set-program sound program)
      (let ((resolver (opusmodus-function "GET-SOUND-SET-PROGRAM")))
        (and resolver
             (funcall resolver (unquote sound) (unquote program))))))
