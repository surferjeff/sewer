%%%-------------------------------------------------------------------
%%% @author  <jeff@LENOVO>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% Implements one hot potato player.
%%%
%%% @end
%%% Created :  1 Jan 2013 by  <jeff@LENOVO>
%%%-------------------------------------------------------------------
-module(potato_player).

-export([start/1, init/1]).

start(ArgList) ->
    spawn_link(?MODULE, init, [ArgList]).

to_int(Atom) ->
    list_to_integer(atom_to_list(Atom)).

choose_random_player(Players) ->
    PlayerNumber = random:uniform(array:size(Players)),
    array:get(PlayerNumber - 1, Players).

init([MasterNode, SendModule | Atoms]) ->
    {_A, B, C} = random:seed(),
    random:seed(erlang:phash2(node()), B, C),
    if
	SendModule == sewer -> sewer:start_link([dll]);
	true -> ok
    end,
    [_PlayerCount, MessageCount, PayloadSize] = lists:map(fun to_int/1, Atoms),
    {potato_master, MasterNode} ! {self(), ready},
    receive
	{kick, Players} ->
	    loop(MasterNode, SendModule, array:from_list(Players),
		 MessageCount, lists:seq(1, PayloadSize))
    after
	30000 ->
	    timeout
    end.

loop(MasterNode, SendModule, Players, MessageCount, Payload) ->
    case MessageCount of
	0 ->
	    {potato_master, MasterNode} ! done;
	_ ->
	    Player = choose_random_player(Players),
 	    apply(SendModule, send,
 		  [Player, {potato, MessageCount - 1, Payload}])
    end,
    receive 
	{potato, NewMessageCount, NewPayload} ->
	    loop(MasterNode, SendModule, Players, NewMessageCount, NewPayload)
    after
	5000 ->
	    io:format("~w timed out.~n", [node()]),
	    flush(),
	    timeout
    end.

flush() ->
    receive 
	Any ->
	    io:format("Flushing ~w~n", [Any]),
	    flush()
    after 
	0 ->
	    true
    end.
