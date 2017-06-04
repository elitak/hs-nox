#! /usr/bin/env bash

target="./dist/build/noxd/noxd"

trap cleanup SIGINT

cleanup() {
        echo "Cleaning up..."
        pkill "$target"
        exit
}

# Start the daemon once, on initial run, if possible
[[ -x "$target" ]] && "$target" &

while true; do
        IFS=$'\n' sourceFiles=($(find . -iname \*.hs))
        inotifywait -qe modify "${sourceFiles[@]}"

        # Do not leave daemon running, in the case of build failure. This is
        # more sensible than leaving a version running that doesn't have the
        # changes we expected to be effected.
        pkill noxd
        cabal build && { "$target" & }
done
