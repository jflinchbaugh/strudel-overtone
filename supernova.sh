#!/bin/sh

pw-jack supernova -u 57110 &
supernova_pid=$!

# Auto-connect supernova to system playback
(
  sleep 2
  if command -v jack_connect >/dev/null 2>&1; then
    pw-jack jack_connect supernova:output_1 system:playback_1
    pw-jack jack_connect supernova:output_2 system:playback_2
    echo "Connected supernova to system playback."
  else
    echo "Warning: 'jack_connect' not found. Please install jack-tools/jack-example-tools or connect manually."
  fi
) &

# pw-jack clj -M:repl/rebel
pw-jack clojure -Sdeps \{\:deps\ \{nrepl/nrepl\ \{\:mvn/version\ \"1.5.2\"\}\ cider/cider-nrepl\ \{\:mvn/version\ \"0.58.0\"\}\ refactor-nrepl/refactor-nrepl\ \{\:mvn/version\ \"3.11.0\"\}\}\ \:aliases\ \{\:cider/nrepl\ \{\:main-opts\ \[\"-m\"\ \"nrepl.cmdline\"\ \"--middleware\"\ \"\[refactor-nrepl.middleware/wrap-refactor\,cider.nrepl/cider-middleware\]\"\]\}\}\} -M:dev:cider/nrepl

kill $supernova_pid
