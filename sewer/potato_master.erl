%%%-------------------------------------------------------------------
%%% @author  <jeff@LENOVO>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% Run the hot pototo game with native erlang sockets,
%%% with 10 players, 5000 potato passes, and a 100-byte potato.
%%% "C:\Program Files (x86)\erl6.2\bin\erl.exe" -sname socket_master -s potato_master init erlang 10 5000 100
%%%
%%% Run the hot pototo game with windows pipes,
%%% with 10 players, 5000 potato passes, and a 100-byte potato.
%%% "C:\Program Files (x86)\erl6.2\bin\erl.exe" -sname pipe_master -s potato_master init sewer 10 5000 100
%%%
%%% @end
%%% Created :  1 Jan 2013 by  <jeff@LENOVO>
%%%-------------------------------------------------------------------
-module(potato_master).

-export([init/1]).

to_int(Atom) ->
    list_to_integer(atom_to_list(Atom)).

init([SendModule | Atoms]) ->
    sewer:start_link([dll]),
    [PlayerCount, MessageCount, PayloadSize] = lists:map(fun to_int/1, Atoms),
    register(potato_master, self()),
    lists:foreach(fun(PlayerNumber) ->
			   start_player(SendModule, PlayerNumber, PlayerCount,
					MessageCount, PayloadSize) end,
		  lists:seq(1, PlayerCount)),
    Players = wait_for_ready(PlayerCount, []),
    lists:foreach(fun(Player) -> Player ! {kick, Players} end, Players),
    {Microseconds, _Result} = timer:tc(fun() -> wait_for_done(PlayerCount) end),
    io:format("Time in milliseconds: ~w~n", [Microseconds div 1000]).

wait_for_ready(0, Players) ->
    Players;
wait_for_ready(PlayerCount, Players) ->
    receive
	{From, ready} ->
	    io:format("Player~w is ready.~n", [PlayerCount]),
	    wait_for_ready(PlayerCount - 1, [From | Players])
    after
	5000 ->
	    timeout
    end.
		 
wait_for_done(0) ->
    ok;
wait_for_done(PlayerCount) ->
    receive
	done ->
	    wait_for_done(PlayerCount - 1)
    end.

start_player(SendModule, PlayerNumber, PlayerCount, MessageCount,
	     PayloadSize) ->
    SlaveName = list_to_atom(lists:flatten(io_lib:format("player~w",
							 [PlayerNumber]))),
    Arg = io_lib:format("-hidden -s potato_player start ~w ~w ~w ~w ~w",
			[node(), SendModule, PlayerCount,
			 MessageCount, PayloadSize]),
    slave:start(net_adm:localhost(), SlaveName, lists:flatten(Arg)).
    
