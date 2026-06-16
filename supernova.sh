#!/bin/sh

# Launch supernova
pw-jack supernova -u 57110 -m 131072 2>&1 > supernova.log &
supernova_pid=$!

# Wait for supernova ports and connect to default sink
(
    for i in $(seq 1 100); do
        if pw-link -o 2>/dev/null | grep -q "^supernova:output_1$"; then
            sink=$(wpctl inspect @DEFAULT_SINK@ | \
                grep 'node.name' | cut -d'"' -f2)
            if [ -n "$sink" ]; then
                pw-link supernova:output_1 "$sink:playback_FL" 2>/dev/null
                pw-link supernova:output_2 "$sink:playback_FR" 2>/dev/null
            fi
            break
        fi
        sleep 0.1
    done
) &

# Start clojure with necessary dependencies
# and run initialization before starting nREPL
pw-jack clojure -Sdeps '{:deps {nrepl/nrepl {:mvn/version "1.5.2"} cider/cider-nrepl {:mvn/version "0.58.0"} refactor-nrepl/refactor-nrepl {:mvn/version "3.11.0"}}}' -M:dev -e "(do (require 'strudel-overtone.core) (strudel-overtone.core/-main) (require 'nrepl.cmdline) (nrepl.cmdline/-main \"--middleware\" \"[refactor-nrepl.middleware/wrap-refactor,cider.nrepl/cider-middleware]\"))" 2>&1 | tee overtone.log

kill $supernova_pid
