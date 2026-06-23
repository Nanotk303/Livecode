;;; Evaluate this file in Opusmodus after loading ../load.lisp.

(setf p1 '(q c4 mf e d4 e e4 q g4)
      p2 '(e c3 p e g3 e bb3 e d4)
      p3 '(h c5 mp h g4))

(live '((inst1 :omn p1 :port "Bus 1" :channel 1
               :sound 'gm :program 8)
        (inst2 :omn p2 :port "Bus 1" :channel 2
               :sound 'gm :program 3
               :controllers (1 '((127)) 11 '((55))))
        (inst3 :omn p3 :port "Bus 1" :channel 3
               :sound 'gm :program 10))
      :tempo 65
      :quantize :bar
      :beats-per-bar 4)

;;; Re-evaluate while the first scene plays. The new material is installed
;;; at the next loop boundary; the clock process is not restarted.
(setf p1 '(e c5 f e d5 e g5 e e5))

(live '((inst1 :omn p1 :port "Bus 1" :channel 1
               :sound 'gm :program 8)
        (inst2 :omn p2 :port "Bus 1" :channel 2
               :sound 'gm :program 3))
      :tempo 92
      :quantize :beat)

;;; Chords are preserved as simultaneous MIDI notes.
(setf chords '((w d4f4a4e5) (w bb3f4ab4d5)))

(live '((harmony :omn chords :port "Bus 1" :channel 4))
      :tempo 72
      :quantize :bar
      :midi-clock-port "Bus 2")

;;; (live-status)
;;; (stop-live)
