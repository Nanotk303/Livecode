# Ableton Link synchronization

Livecode can optionally follow an Ableton Link session. This makes it possible
to synchronize Livecode with Ableton Live or with other Livecode users on other
machines.

The Link integration uses a small native bridge:

```text
native/build/liblivecode-link.dylib
```

Livecode itself remains Common Lisp; the bridge wraps the official Ableton Link
C++ SDK behind a small C ABI that LispWorks can call through FLI.

## Build the bridge

From the repository root:

```sh
cmake -S native -B native/build
cmake --build native/build
```

If you already have the Ableton Link SDK checked out locally:

```sh
git clone --recursive https://github.com/Ableton/link.git /tmp/ableton-link
cmake -S native -B native/build -DABLETON_LINK_DIR=/tmp/ableton-link
cmake --build native/build
```

## Load the bridge in Opusmodus

```lisp
(load "/path/to/Livecode/load.lisp")

(livecode:load-ableton-link
 "/path/to/Livecode/native/build/liblivecode-link.dylib")
```

Check status:

```lisp
(livecode:ableton-link-status)
```

Optionally send Link start/stop intentions:

```lisp
(livecode:ableton-link-start :quantum 4)
(livecode:ableton-link-stop)
```

## Use Link from `live`

```lisp
(live '((synth1 :omn p1 :port "Bus 1" :channel 1)
        (kick   :omn kick :port "Bus 1" :channel 3))
      :tempo 120
      :link t
      :link-quantum 4
      :link-start-stop t)
```

`link-quantum` is the Link phase quantum in quarter-note beats. A value of `4`
means that Livecode will align swaps and cycle starts to a four-beat Link
boundary.

## Suggested two-computer jam setup

On both computers:

```lisp
(load "/path/to/Livecode/load.lisp")
(livecode:load-ableton-link
 "/path/to/Livecode/native/build/liblivecode-link.dylib")
```

Then each performer can run their own material:

```lisp
(live '((lead :omn p1 :port "Bus 1" :channel 1))
      :tempo 120
      :link t
      :link-quantum 4)
```

If Ableton Live is also open on the same network with Link enabled, Livecode and
Live will share the same Link session.

## Start/stop sync

Livecode can participate in Link start/stop sync:

```lisp
(live ... :link t :link-start-stop t)
```

Not every Link application treats start/stop exactly the same way, but tempo,
beat and phase synchronization are the essential shared timeline.

## License note

Ableton Link is distributed by Ableton under GPLv2+ or a proprietary license.
Livecode is MIT licensed, but building and distributing the optional native
bridge together with Ableton Link may carry additional GPL obligations. See the
Ableton Link license for details.
