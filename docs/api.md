# API reference

This is a compact overview of the user-facing API.

## Main commands

```lisp
(live tracks &key tempo quantize beats-per-bar midi-clock-port
                  link link-quantum link-start-stop)
(stop-live)
(panic)
(live-status)
(live-last-error)
```

## MIDI backend

```lisp
(use-direct-midi)
(use-opusmodus-midi)
(use-logging-midi)
(set-midi-sender function)
(set-midi-realtime-sender function)
(list-midi-destinations)
```

## Tests and diagnostics

```lisp
(test-midi-output :port "Bus 1")
(test-midi-timestamped-output :port "Bus 1")
(test-midi-clock-output :port "Bus 2" :tempo 75 :seconds 2)
(test-midi-mts-output :port "Bus 1" :note 60.5d0)
(midi-clock-diagnostics)
```

## Inspection

```lisp
(live-inspect tracks :tempo 120)
(live-articulation-inspect tracks :tempo 120)
```

## Timing presets

```lisp
(use-livecode-safe-timing)
(use-livecode-rock-solid-timing :ahead 0.35d0)
```

## Ableton Link

```lisp
(load-ableton-link "/path/to/liblivecode-link.dylib")
(use-ableton-link :tempo 120 :quantum 4 :start-stop t)
(ableton-link-status)
(ableton-link-start :quantum 4)
(ableton-link-stop)
(stop-ableton-link)
```

Use Link from `live`:

```lisp
(live tracks :tempo 120 :link t :link-quantum 4)
```

## Soundsets

```lisp
(reload-livecode-soundsets)
(set-livecode-soundset-directory "/path/to/Def-Sound-Sets/")
(live-soundset-status)
(live-soundset-program 'soundset-name 'articulation-name)
```

## Important parameters

```lisp
livecode:*event-schedule-ahead-seconds*
livecode:*event-wakeup-resolution-seconds*
livecode:*timestamp-midi-events*
livecode:*midi-clock-event-offset-seconds*
livecode:*retrigger-note-off-advance-beats*
livecode:*mts-enabled*
livecode:*sound-set-directory*
livecode:*sound-set-auto-reload*
livecode:*send-redundant-articulation-messages*
livecode:*ableton-link-library-path*
livecode:*ableton-link-default-quantum*
livecode:*ableton-link-start-stop-sync*
```
