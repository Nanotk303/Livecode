# Ableton Link bridge

Livecode uses an optional native bridge to participate in Ableton Link
sessions.

## Build

With CMake fetching the Ableton Link SDK:

```sh
cmake -S native -B native/build
cmake --build native/build
```

Or with a local Ableton Link checkout:

```sh
git clone --recursive https://github.com/Ableton/link.git /tmp/ableton-link
cmake -S native -B native/build -DABLETON_LINK_DIR=/tmp/ableton-link
cmake --build native/build
```

The resulting library should be:

```text
native/build/liblivecode-link.dylib
```

## Load in Opusmodus

```lisp
(load "/path/to/Livecode/load.lisp")
(livecode:load-ableton-link
 "/path/to/Livecode/native/build/liblivecode-link.dylib")
```

Then:

```lisp
(live '((lead :omn p1 :port "Bus 1" :channel 1))
      :tempo 120
      :link t
      :link-quantum 4
      :link-start-stop t)
```

## License note

Ableton Link is distributed by Ableton under GPLv2+ or a proprietary license.
Livecode itself is MIT licensed, but building and distributing the optional
bridge with Ableton Link may carry additional GPL obligations. See the Ableton
Link license for details.
