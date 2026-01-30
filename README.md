# strudel-overtone

My music explorations in Clojure and Overtone
making it look a little like Strudel.

## Running the Workstation

* Ensure `pipewire` is running to serve as our JACK server
* Start supernova and an nrepl: `./supernova.sh`
* The script will try to wire supernova to a system output in pipewire,
  but sometimes it needs to be checked in `qpwgraph`
* In Emacs:
 * Start emacs
 * _connect_ to the running repl
 * evaluate all of `strudel-overtone/strudel-overtone.clj`
 * you should be connected and ready to go!


## Running tests

`$ clj -T:build test`

