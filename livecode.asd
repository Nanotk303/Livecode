(asdf:defsystem #:livecode
  :description "Minimal live-coding engine for Opusmodus / LispWorks."
  :author "Livecode contributors"
  :license "MIT"
  :version "0.1.0"
  :in-order-to ((asdf:test-op (asdf:test-op #:livecode/tests)))
  :serial t
  :components ((:file "src/package")
               (:file "src/model")
               (:file "src/platform")
               (:file "src/link")
               (:file "src/omn")
               (:file "src/soundsets")
               (:file "src/midi")
               (:file "src/engine")
               (:file "src/api")))

(asdf:defsystem #:livecode/tests
  :depends-on (#:livecode)
  :serial t
  :components ((:file "tests/tests"))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :livecode.tests :run-tests)))
