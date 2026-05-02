#!/bin/sh

# Launch supernova
pw-jack supernova -u 57110 -m 131072 2>&1 > supernova.log &
supernova_pid=$!

# Start clojure with necessary dependencies
# and run initialization before starting nREPL
pw-jack clojure -Sdeps '{:deps {nrepl/nrepl {:mvn/version "1.5.2"} cider/cider-nrepl {:mvn/version "0.58.0"} refactor-nrepl/refactor-nrepl {:mvn/version "3.11.0"}}}' -M:dev -e "(do (require 'strudel-overtone.core) (strudel-overtone.core/-main) (require 'nrepl.cmdline) (nrepl.cmdline/-main \"--middleware\" \"[refactor-nrepl.middleware/wrap-refactor,cider.nrepl/cider-middleware]\"))" 2>&1 | tee overtone.log

kill $supernova_pid
