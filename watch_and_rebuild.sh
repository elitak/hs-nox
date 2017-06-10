#! /usr/bin/env bash

# TODO detect a second change to files and abort slow-running cabal-build and restart it
# TODO nixify and include as part of nix-build's output?
# TODO put this in hs-nix-template once fully generalized

# TODO detect target by parsing *.cabal
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
        pkill $(basename $target)
        cabal build && { "$target" & }
done
