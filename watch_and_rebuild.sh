#! /usr/bin/env bash
while true; do
        inotifywait -qe modify $(find . -iname \*.hs)
        pkill noxd
        cabal build && ./dist/build/noxd/noxd &
done
