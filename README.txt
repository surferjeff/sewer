How to build and run the example:
1.  Edit make.bat and set the ERL32 variables to point to your installation of 32-bit Erlang. 
2.  Start a Visual Studio Command Prompt.
3.  In the Visual Studio window, cd into this directory, then run:
    make
    run

  Sewer is an experiment to use Windows Pipes instead of sockets as the transport for erlang messaging.  The current code proves the concept.

Why would anyone want Windows Pipes instead of sockets?
1.  Running in an enterprise environment where Erlang's cookie's do not integrate with existing authentication and security.
2.  Want to run on a user's machine, and don't want the Firewall warning to pop up.  The user may not even have permission to poke a hole in the firewall.

This is not yet a useable solution.  Remaining tasks:
1.  Integrate it with Erlang's messaging layer.  It's presently implemented as a separate module.
2.  Make it work across machines.  I have a PipeBroker-to-PipeBroker connection pair scheme in mind.

Have fun!
-Jeffrey Rennie

