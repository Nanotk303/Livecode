# MTS microtuning

Livecode can send MIDI Tuning Standard Single Note Tuning Change messages for
microtonal pitches.

MTS support is enabled by default:

```lisp
(setf livecode:*mts-enabled* t)
```

Useful parameters:

```lisp
livecode:*mts-device-id*
livecode:*mts-tuning-program*
livecode:*mts-lead-time-seconds*
```

Test MTS output:

```lisp
(livecode:test-midi-mts-output
 :port "Bus 1"
 :channel 1
 :note 60.5d0)
```

The receiving instrument must support MTS. Some Vienna / Synchron instruments
can receive MTS, depending on configuration.

If the instrument does not support MTS, disable it:

```lisp
(setf livecode:*mts-enabled* nil)
```
