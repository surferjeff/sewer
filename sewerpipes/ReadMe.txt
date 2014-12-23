SewerPipes implements the PipeBroker.  It functions exactly like epmd, but for Windows named pipes.
The PipeBroker maintains a map for all node names on the localhost to their corresponding pipes.

Nodes connect with either:
  {visible, Pid}
or
  {hidden, Pid}
messages.

The Pid is the process id of a dedicated dispatcher process running in the erlang node.

The PipeBroker then holds on to that pipe and hands it to other nodes that are looking for
it with getpipe requests.

For all other commands from the erlang node to the PipeBroker, the pipe is short lived.
It lives for only one command, and then the PipeBroker closes it.

So there are long-lived pipes pointing back to the nodes,
and short-lived pipes for requests from the nodes to the PipeBroker.

