# Installation

Livecode is intended to be loaded from Opusmodus / LispWorks.

## Requirements

- macOS
- Opusmodus
- LispWorks, as used by Opusmodus
- A MIDI destination, usually an IAC bus

## Loading

Evaluate:

```lisp
(load "/path/to/Livecode/load.lisp")
```

The short user commands are installed in the usual Opusmodus packages, so after
loading you can usually call:

```lisp
(live ...)
(stop-live)
(live-status)
```

If you prefer package-qualified names:

```lisp
(livecode:live ...)
(livecode:stop-live)
(livecode:live-status)
```

## MIDI setup

Create or enable an IAC bus in macOS Audio MIDI Setup, then use its name in
`:port`:

```lisp
(live '((inst1 :omn p1 :port "Bus 1" :channel 1))
      :tempo 90)
```

To inspect MIDI destinations:

```lisp
(livecode:list-midi-destinations)
```

To test a note without starting Livecode:

```lisp
(livecode:test-midi-output :port "Bus 1")
```

## Optional timing preset

The default timing can already work well in many setups. If you hear timing
instability under heavier loads, enable the ahead-scheduled timing preset:

```lisp
;; Optional: use this if you hear timing instability under heavier loads.
(use-livecode-rock-solid-timing :ahead 0.35d0)
(setf livecode:*retrigger-note-off-advance-beats* 0)
(setf livecode:*midi-clock-event-offset-seconds* 0d0)
```
