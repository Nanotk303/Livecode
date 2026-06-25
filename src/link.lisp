(in-package #:livecode)

(defun default-ableton-link-library-path ()
  (let ((source (or *load-truename* *compile-file-truename*)))
    (if source
        (merge-pathnames #P"../native/build/liblivecode-link.dylib"
                         source)
        #P"native/build/liblivecode-link.dylib")))

(defparameter *ableton-link-library-path*
  (default-ableton-link-library-path)
  "Default path of the optional Ableton Link bridge dylib.")

(defparameter *ableton-link-default-quantum* 4
  "Default Ableton Link quantum in quarter-note beats.")

(defparameter *ableton-link-start-stop-sync* t
  "When true, Livecode participates in Ableton Link start/stop sync.")

(defvar *ableton-link-loaded-p* nil)

#+lispworks
(progn
  (fli:define-foreign-function (%lc-link-create "livecode_link_create")
      ((tempo :double))
    :result-type :int
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-destroy "livecode_link_destroy")
      ()
    :result-type :void
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-enable "livecode_link_enable")
      ((enabled :int))
    :result-type :void
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-is-enabled "livecode_link_is_enabled")
      ()
    :result-type :int
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-enable-start-stop
                                "livecode_link_enable_start_stop")
      ((enabled :int))
    :result-type :void
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-is-start-stop-enabled
                                "livecode_link_is_start_stop_enabled")
      ()
    :result-type :int
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-num-peers "livecode_link_num_peers")
      ()
    :result-type :int
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-tempo "livecode_link_tempo")
      ()
    :result-type :double
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-set-tempo "livecode_link_set_tempo")
      ((tempo :double))
    :result-type :void
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-beat "livecode_link_beat")
      ((quantum :double))
    :result-type :double
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-phase "livecode_link_phase")
      ((quantum :double))
    :result-type :double
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-seconds-until-beat
                                "livecode_link_seconds_until_beat")
      ((beat :double)
       (quantum :double))
    :result-type :double
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-is-playing "livecode_link_is_playing")
      ()
    :result-type :int
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-start-playing
                                "livecode_link_start_playing")
      ((quantum :double))
    :result-type :void
    :module "livecode-link")

  (fli:define-foreign-function (%lc-link-stop-playing
                                "livecode_link_stop_playing")
      ()
    :result-type :void
    :module "livecode-link"))

#-lispworks
(progn
  (defun %lc-link-create (tempo) (declare (ignore tempo)) 0)
  (defun %lc-link-destroy () nil)
  (defun %lc-link-enable (enabled) (declare (ignore enabled)) nil)
  (defun %lc-link-is-enabled () 0)
  (defun %lc-link-enable-start-stop (enabled) (declare (ignore enabled)) nil)
  (defun %lc-link-is-start-stop-enabled () 0)
  (defun %lc-link-num-peers () 0)
  (defun %lc-link-tempo () 120d0)
  (defun %lc-link-set-tempo (tempo) (declare (ignore tempo)) nil)
  (defun %lc-link-beat (quantum) (declare (ignore quantum)) 0d0)
  (defun %lc-link-phase (quantum) (declare (ignore quantum)) 0d0)
  (defun %lc-link-seconds-until-beat (beat quantum)
    (declare (ignore beat quantum))
    0d0)
  (defun %lc-link-is-playing () 0)
  (defun %lc-link-start-playing (quantum) (declare (ignore quantum)) nil)
  (defun %lc-link-stop-playing () nil))

(defun link-boolean (value)
  (if value 1 0))

(defun link-true-p (value)
  (not (zerop value)))

(defun load-ableton-link (&optional (library *ableton-link-library-path*))
  "Load the optional native Ableton Link bridge.

Build the bridge in native/ first; then call this function before using
`(live ... :link t)`."
  #+lispworks
  (let ((library (truename library)))
    (fli:register-module "livecode-link"
                         :real-name (namestring library)
                         :connection-style :immediate)
    (setf *ableton-link-loaded-p* t
          *ableton-link-library-path* library)
    (namestring library))
  #-lispworks
  (progn
    (format nil "~A" library)
    (error "Ableton Link bridge loading is currently implemented for LispWorks/Opusmodus.")))

(defun ableton-link-loaded-p ()
  *ableton-link-loaded-p*)

(defun ensure-ableton-link-loaded ()
  (unless (ableton-link-loaded-p)
    (when (probe-file *ableton-link-library-path*)
      (ignore-errors (load-ableton-link *ableton-link-library-path*))))
  (unless (ableton-link-loaded-p)
    (error "Ableton Link is enabled for this Livecode scene, but the native bridge is not loaded. Build native/liblivecode-link.dylib, then evaluate:~%  (livecode:load-ableton-link \"/path/to/liblivecode-link.dylib\")")))

(defun use-ableton-link (&key (tempo 120)
                              (quantum *ableton-link-default-quantum*)
                              (start-stop *ableton-link-start-stop-sync*)
                              library)
  "Enable Ableton Link and make Livecode join the current Link session."
  (when library
    (load-ableton-link library))
  (ensure-ableton-link-loaded)
  (unless (and (realp tempo) (plusp tempo))
    (error "Ableton Link tempo must be positive, got ~S." tempo))
  (unless (and (realp quantum) (plusp quantum))
    (error "Ableton Link quantum must be positive, got ~S." quantum))
  (unless (link-true-p (%lc-link-create (coerce tempo 'double-float)))
    (error "Could not create the Ableton Link session."))
  (%lc-link-enable 1)
  (%lc-link-enable-start-stop (link-boolean start-stop))
  (%lc-link-set-tempo (coerce tempo 'double-float))
  (list :enabled (link-true-p (%lc-link-is-enabled))
        :tempo (%lc-link-tempo)
        :quantum quantum
        :peers (%lc-link-num-peers)
        :start-stop-sync
        (link-true-p (%lc-link-is-start-stop-enabled))
        :playing (link-true-p (%lc-link-is-playing))))

(defun stop-ableton-link ()
  "Disable the Ableton Link bridge."
  (when (ableton-link-loaded-p)
    (ignore-errors (%lc-link-enable 0))
    (ignore-errors (%lc-link-destroy)))
  :stopped)

(defun ableton-link-status (&key (quantum *ableton-link-default-quantum*))
  "Return the current Ableton Link status."
  (if (not (ableton-link-loaded-p))
      (list :loaded nil
            :library (namestring *ableton-link-library-path*))
      (list :loaded t
            :library (namestring *ableton-link-library-path*)
            :enabled (link-true-p (%lc-link-is-enabled))
            :peers (%lc-link-num-peers)
            :tempo (%lc-link-tempo)
            :quantum quantum
            :beat (%lc-link-beat (coerce quantum 'double-float))
            :phase (%lc-link-phase (coerce quantum 'double-float))
            :start-stop-sync
            (link-true-p (%lc-link-is-start-stop-enabled))
            :playing (link-true-p (%lc-link-is-playing)))))

(defun ableton-link-start (&key (quantum *ableton-link-default-quantum*))
  "Send a Link start-playing intention aligned to QUANTUM."
  (ensure-ableton-link-loaded)
  (%lc-link-start-playing (coerce quantum 'double-float))
  (ableton-link-status :quantum quantum))

(defun ableton-link-stop ()
  "Send a Link stop-playing intention."
  (ensure-ableton-link-loaded)
  (%lc-link-stop-playing)
  (ableton-link-status))

(defun ableton-link-tempo ()
  (%lc-link-tempo))

(defun ableton-link-beat (&optional (quantum *ableton-link-default-quantum*))
  (%lc-link-beat (coerce quantum 'double-float)))

(defun ableton-link-seconds-until-beat
    (beat &optional (quantum *ableton-link-default-quantum*))
  (%lc-link-seconds-until-beat (coerce beat 'double-float)
                               (coerce quantum 'double-float)))

(defun ableton-link-next-beat-boundary (quantum beat)
  (* quantum (ceiling (/ beat quantum))))

(defun ableton-link-local-time-for-beat
    (beat &optional (quantum *ableton-link-default-quantum*))
  (+ (monotonic-seconds)
     (ableton-link-seconds-until-beat beat quantum)))

(defun ableton-link-next-quantized-time (quantum &optional minimum-time)
  (let* ((quantum (coerce quantum 'double-float))
         (beat (ableton-link-beat quantum))
         (target-beat (ableton-link-next-beat-boundary quantum beat))
         (target-time (ableton-link-local-time-for-beat target-beat quantum)))
    (if (and minimum-time (< target-time minimum-time))
        (ableton-link-local-time-for-beat (+ target-beat quantum) quantum)
        target-time)))

(defun configure-link-for-scene (scene)
  (when (scene-link-enabled scene)
    (use-ableton-link :tempo (scene-tempo scene)
                      :quantum (scene-link-quantum scene)
                      :start-stop (scene-link-start-stop scene))
    (setf (scene-tempo scene) (ableton-link-tempo))))

(defun start-link-transport-for-scene (scene)
  "Send a Link start intention when SCENE asks for start/stop sync."
  (when (and scene
             (scene-link-enabled scene)
             (scene-link-start-stop scene))
    (ableton-link-start :quantum (scene-link-quantum scene))))

(defun stop-link-transport-for-scene (scene)
  "Send a Link stop intention when SCENE asks for start/stop sync."
  (when (and scene
             (scene-link-enabled scene)
             (scene-link-start-stop scene))
    (ableton-link-stop)))

(defun refresh-link-tempo-for-scene (scene)
  (when (and scene (scene-link-enabled scene))
    (setf (scene-tempo scene) (ableton-link-tempo))))

(defun link-scene-cycle-start-time (scene &optional minimum-time)
  (ableton-link-next-quantized-time (scene-length scene) minimum-time))
