Code or Die Client

To read this code:

Start with `components.rkt`. components.rkt provides the function `make-pipeline`,
which accepts a list of functions. The first function must take no input and produce some number
of values. Then, for each following function, the values produced by the previous function
are given as inputs, and each value output by the current function is passed to the next one.
Each set of values is sent to the following functions by serializing the value into a JSONString,
and putting the JSONString into a redis list. Each function, in a thread, will BLPOP (blocking pop,
which waits for 3 seconds to try and receive a value) the redis list, receiving one value. (If the
list is empty, the thread will wait another three seconds and try again.) It calls its function on
the single JSExpr it was given, and then produces a list of values to be passed on to the next function.
This process repeats until you hit the last function, which does not produce any output. In the example
use of this framework for the code-or-die client, the final functions do side-effectful operations,
such as storing data in a database or sending PUT, POST, or DELETE requests to the API. 

`xclient.rkt` will run the client, including a visualization of each pipeline. 

`storage.rkt` provides utilities to store data into a mongo database, including a macro to register
a collection (named, hopefully unsurprisingly, `register-collection`), providing functions to
store and retrieve data in that collection.

`messages.rkt` implements message-passing, allowing the clients to talk to each other. It
allows support for multiple redis servers (although the interface does not need to know
which message passing database is being used under the hood) with a paramater. Whenever
a new message channel is spun up, it is chosen randomly from the `message-broker-hostnames`.

In `xclient.rkt`, `setup.rkt` reads `config.json` and then sets global variables. `test-config.json`
uses only the default redis server, so that all messages are passed in the same place for easier debugging.

`operations.rkt` is application specific, and contains many functions that are useful for people
trying to play code-or-die.
