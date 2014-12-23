cd sewer
:: Run the hot pototo game with native erlang sockets,
:: with 10 players, 5000 potato passes, and a 100-byte potato.
:: "%ERL32_BIN%\erl" -sname socket_master -s potato_master run erlang 10 5000 100
::
:: Run the hot pototo game with windows pipes,
:: with 10 players, 5000 potato passes, and a 100-byte potato.
"%ERL32_BIN%\erl" -sname pipe_master -s potato_master run sewer 10 5000 100
cd ..