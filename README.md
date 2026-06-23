# Livecode

Livecode is a small Common Lisp live-coding prototype for
[Opusmodus](https://opusmodus.com/) / LispWorks.

It is conceptually inspired by pattern-oriented environments such as Strudel,
but keeps an idiomatic Lisp/OMN workflow:

```lisp
(live '((synth1 :omn p1   :port "Bus 1" :channel 1)
        (synth2 :omn pad1 :port "Bus 1" :channel 2)
        (kick   :omn kick :port "Bus 1" :channel 3))
      :tempo 120
      :midi-clock-port "Bus 2")
```

The project is currently an experimental prototype. It is already useful for
testing live replacement of OMN material, direct CoreMIDI output, MIDI clock,
MTS microtuning and Opusmodus soundset articulations, but the API may still
change.

## Features

- Evaluate `(live ...)` expressions directly from Opusmodus / LispWorks.
- Keep a global clock running while replacing the musical material.
- Parse common OMN material through Opusmodus when available.
- Send MIDI directly to named macOS IAC/CoreMIDI destinations.
- Schedule MIDI events ahead with timestamped CoreMIDI output for stable timing.
- Optionally send MIDI Clock to a separate destination.
- Support MTS Single Note Tuning Change messages for microtonal pitches.
- Read Opusmodus `def-sound-set` files and use OMN articulations as
  soundset keyswitches / controllers / program changes.
- Support named soundset controllers such as `expression` or `velocity-xf`.

## Quick start

1. Download or clone this repository.
2. In Opusmodus, evaluate:

   ```lisp
   (load "/path/to/Livecode/load.lisp")
   ```

3. Optional: if you hear timing instability under heavier loads, enable the
   ahead-scheduled timing preset:

   ```lisp
   ;; Optional: use this if you hear timing instability under heavier loads.
   (use-livecode-rock-solid-timing :ahead 0.35d0)
   ```

4. Define some material:

   ```lisp
   (setf p1 '((q c4 e d4 e e4 q g4)))
   ```

5. Start Livecode:

   ```lisp
   (live '((inst1 :omn p1 :port "Bus 1" :channel 1))
         :tempo 90)
   ```

6. Replace the pattern while the clock keeps running:

   ```lisp
   (setf p1 '((s c4 d4 e4 g4 q c5)))

   (live '((inst1 :omn p1 :port "Bus 1" :channel 1))
         :tempo 90)
   ```

Stop everything:

```lisp
(stop-live)
```

Emergency all-notes-off:

```lisp
(panic)
```

## Documentation

- [Installation](docs/installation.md)
- [Live coding workflow](docs/live-coding.md)
- [Timing and MIDI Clock](docs/timing.md)
- [Opusmodus soundsets](docs/soundsets.md)
- [MTS microtuning](docs/mts.md)
- [API reference](docs/api.md)

## Opusmodus soundsets

Livecode tries to find the usual Opusmodus soundset folder automatically:

```text
~/Opusmodus/User Source/Libraries/Def-Sound-Sets/
~/Documents/Opusmodus/User Source/Libraries/Def-Sound-Sets/
```

If your Opusmodus installation is elsewhere:

```lisp
(livecode:set-livecode-soundset-directory
 "/path/to/Opusmodus/User Source/Libraries/Def-Sound-Sets/")
```

Check what Livecode loaded:

```lisp
(livecode:live-soundset-status)
```

## Example with soundset articulations

```lisp
(setf vln
      '((s d4 p< leg
          e4 <  leg
          f4 <  leg
          g4 <  def
          q a4 f marc
          h b4 ff trem+ponte)))

(live '((violin :omn vln
                :port "Bus 1"
                :channel 1
                :sound 'vsl-studio-solo-violin1
                :program '(def)))
      :tempo 120)
```

Repeated articulations are not resent by default. For instruments that require
articulation messages to be reasserted on every note:

```lisp
(setf livecode:*send-redundant-articulation-messages* t)
```

## Status

This is a first functional version, built through practical testing with
Opusmodus, LispWorks, macOS IAC buses, Vienna Ensemble Pro and Synchron Player.
Expect rough edges, especially around the full breadth of OMN syntax and
third-party soundset conventions.

## License

MIT. See [LICENSE](LICENSE).
