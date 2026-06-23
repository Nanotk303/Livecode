# Opusmodus soundsets

Livecode can scan Opusmodus `def-sound-set` files and use those definitions
while compiling OMN articulations.

## Automatic discovery

Livecode searches common Opusmodus locations:

```text
~/Opusmodus/User Source/Libraries/Def-Sound-Sets/
~/Documents/Opusmodus/User Source/Libraries/Def-Sound-Sets/
```

It also checks the environment variables:

```text
LIVECODE_SOUND_SET_DIRECTORY
OPUSMODUS_SOUND_SET_DIRECTORY
```

## Manual location

If automatic discovery fails:

```lisp
(livecode:set-livecode-soundset-directory
 "/path/to/Opusmodus/User Source/Libraries/Def-Sound-Sets/")
```

Check status:

```lisp
(livecode:live-soundset-status)
```

## Articulations

Given an OMN phrase:

```lisp
(setf vln
      '((s d4 p< leg
          e4 <  leg
          f4 <  leg
          g4 <  def
          q a4 f marc
          h b4 ff trem+ponte)))
```

And a matching soundset:

```lisp
(live '((violin :omn vln
                :port "Bus 1"
                :channel 1
                :sound 'vsl-studio-solo-violin1
                :program '(def)))
      :tempo 120)
```

Livecode resolves articulations such as `leg`, `marc` or `trem+ponte` through
the soundset. Supported messages include:

- keyswitches
- controller changes
- bank select
- program change

Repeated articulations are not resent by default:

```lisp
(setf livecode:*send-redundant-articulation-messages* nil)
```

For instruments that need articulation state reasserted on every note:

```lisp
(setf livecode:*send-redundant-articulation-messages* t)
```

## Named controllers

If a soundset defines named controllers, they can be used directly:

```lisp
(live '((violin :omn vln
                :port "Bus 1"
                :channel 1
                :sound 'vsl-studio-solo-violin1
                :controllers (expression '(90)
                              velocity-xf '((0 1/128)
                                            (127 1/128)))))
      :tempo 120)
```

## Debugging

Inspect resolved articulations:

```lisp
(livecode:live-articulation-inspect
 '((violin :omn vln
           :port "Bus 1"
           :channel 1
           :sound 'vsl-studio-solo-violin1
           :program '(def)))
 :tempo 120)
```
