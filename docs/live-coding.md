# Live coding workflow

The basic workflow is:

1. Define or generate OMN material in variables.
2. Evaluate a `(live ...)` form.
3. Change the variables or the live form.
4. Evaluate `(live ...)` again.

Livecode keeps the global engine running and swaps in the new scene at a safe
musical boundary.

## Minimal example

```lisp
(setf p1 '((q c4 e d4 e e4 q g4)))

(live '((lead :omn p1 :port "Bus 1" :channel 1))
      :tempo 90)
```

Replace the material:

```lisp
(setf p1 '((s c4 d4 e4 g4 q c5)))

(live '((lead :omn p1 :port "Bus 1" :channel 1))
      :tempo 90)
```

## Several tracks

```lisp
(live '((synth1 :omn p1   :port "Bus 1" :channel 1)
        (synth2 :omn pad1 :port "Bus 1" :channel 2)
        (kick   :omn kick :port "Bus 1" :channel 3))
      :tempo 120)
```

## Quantized replacement

Livecode supports quantized replacement, for example at the bar:

```lisp
(live '((lead :omn p1 :port "Bus 1" :channel 1))
      :tempo 120
      :quantize :bar
      :beats-per-bar 4)
```

## Status and inspection

```lisp
(live-status)
(live-last-error)
(live-inspect '((lead :omn p1 :port "Bus 1" :channel 1)) :tempo 120)
```
