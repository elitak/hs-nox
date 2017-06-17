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

updateBuild() {
        local nextBuild
        nextBuild=$(grep -E '^ *version:' ./*.cabal | awk '{print $2}' | awk -F'.' '{print $1"."$2"."$3"."++$4}')
        sed -ri ./*.cabal -e "s/^( *version: *)[0-9.]+$/\1$nextBuild/"
        touch ./src/Version.hs # Any files using cabal-file-th be touched so that TH regens any strings pulled from cabal
}

# Start the daemon once, on initial run, if possible
[[ -x "$target" ]] && "$target" &

while true; do
        IFS=$'\n' sourceFiles=($(find . -iname \*.hs))
        inotifywait -qe modify "${sourceFiles[@]}"

        # Do not leave daemon running, in the case of build failure. This is
        # more sensible than leaving a version running that doesn't have the
        # changes we expected to be effected.
        pkill "$(basename $target)"
        updateBuild
        cabal build && { "$target" & }
done
