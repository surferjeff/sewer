%% A silly benchmark that passes messages between nodes as quickly
%% as possible.
-module(potato).

-export([start/4, loop/1]).

start(SendModule, {RegName, Node}, Count, PayloadSize) ->
    apply(SendModule, send, [{RegName, Node}, {potato, self(), Count,
					       lists:seq(1, PayloadSize)}]),
    loop(SendModule).

loop(SendModule) ->
    receive 
	{potato, _From, 0, _Payload} ->
	    ok;
	{potato, From, Count = 1, Payload} ->
	    apply(SendModule, send, [From, {potato, self(), Count - 1,
					    Payload} ]),
	    ok;
	{potato, From, Count, Payload} ->
	    apply(SendModule, send, [From, {potato, self(), Count - 1,
					    Payload}] ),
	    loop(SendModule)
    after
	10000 -> timeout
    end.
