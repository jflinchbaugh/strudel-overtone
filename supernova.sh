#!/bin/sh

pw-jack supernova -u 57110 &
supernova_pid=$!

# pw-jack clj -M:repl/rebel
pw-jack clojure -Sdeps \{\:deps\ \{nrepl/nrepl\ \{\:mvn/version\ \"1.5.2\"\}\ cider/cider-nrepl\ \{\:mvn/version\ \"0.58.0\"\}\ refactor-nrepl/refactor-nrepl\ \{\:mvn/version\ \"3.11.0\"\}\}\ \:aliases\ \{\:cider/nrepl\ \{\:main-opts\ \[\"-m\"\ \"nrepl.cmdline\"\ \"--middleware\"\ \"\[refactor-nrepl.middleware/wrap-refactor\,cider.nrepl/cider-middleware\]\"\]\}\}\} -M:dev:cider/nrepl

kill $supernova_pid
