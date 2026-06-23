# Timing and MIDI Clock

Livecode can send ordinary MIDI events directly, and can also send MIDI Clock
to a separate port.

## Stable scheduling

The default timing can already be accurate enough in many setups. If you hear
timing instability under heavier loads, the most stable preset schedules MIDI
ahead of time and uses CoreMIDI timestamps:

```lisp
;; Optional: use this if you hear timing instability under heavier loads.
(use-livecode-rock-solid-timing :ahead 0.35d0)
```

This introduces a small live-coding latency, but gives the MIDI output more
time to be delivered accurately.

Useful timing parameters:

```lisp
livecode:*event-schedule-ahead-seconds*
livecode:*event-wakeup-resolution-seconds*
livecode:*timestamp-midi-events*
```

## MIDI Clock

Add `:midi-clock-port`:

```lisp
(live '((lead :omn p1 :port "Bus 1" :channel 1))
      :tempo 75
      :midi-clock-port "Bus 2")
```

Livecode sends MIDI real-time messages:

- `FA` start
- `F8` clock pulses, 24 per quarter note
- `FC` stop

Test MIDI Clock only:

```lisp
(livecode:test-midi-clock-output
 :port "Bus 2"
 :tempo 75
 :seconds 2)
```

## Synchronizing clock-driven hosts

Some hosts or plug-ins add latency between incoming MIDI Clock and generated
audio. If a clock-driven arpeggiator feels late or early compared with direct
MIDI notes, adjust:

```lisp
(setf livecode:*midi-clock-event-offset-seconds* 0d0)
```

Positive values delay ordinary events relative to the clock; negative values
can move them earlier if the backend supports it safely.
