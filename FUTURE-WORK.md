# Webmachine

Sadly enough, this will not be a finished project before my thesis ends, but
I hope for it to maintain a breeding ground for experiments i have that I can
potentially port to Servant.

One of the cooler things that I have had in mind for a long time is
combinig ideas of https://github.com/webmachine/webmachine and Servant together. My two favorite web frameworks.

In webmachine, a webserver is a finite state machine where each node in the DAG represents some sort of test that moves it further into the FSM.  If you do not implement a node, it uses its default behaviour.

By exposing which nodes of the FSM are implemented in the API type, we can give great and detailed information about what kind of error codes we can expect.  This is great for documentation purposes.

So the idea is: Tag parts of the FSM that are implemented in your handlers in the API type.
