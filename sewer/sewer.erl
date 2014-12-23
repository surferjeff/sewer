%%%-------------------------------------------------------------------
%%% @author  <jeff@LENOVO>
%%% @copyright (C) 2012, 
%%% @doc
%%% 
%%% This library is designed match the API of the erlang module for
%%% all its network/messaging functions.  That way, it can be used as
%%% a drop-in replacement for the erlang module.
%%%
%%% @end
%%% Created : 29 Dec 2012 by  <jeff@LENOVO>
%%%-------------------------------------------------------------------
-module(sewer).

-compile({no_auto_import,[nodes/1, spawn_opt/5, link/1]}).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0, start/0, start/1]).

-export([send/2, connect_node/1, nodes/0, nodes/1,
	 spawn/1, spawn/2, spawn/3, spawn/4,
	 spawn_link/1, spawn_link/2, spawn_link/3, spawn_link/4,
	 spawn_opt/5,
	 link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  port, % The erlang port connected to driver
	  links % A dict mapping local pids to remote pids.  Each pid
	        % pair represents a link.
	 }).

%%%===================================================================
%%% API
%%%===================================================================

send({RegName, Node}, Msg) when is_atom(Node); is_atom(RegName) ->
    if
	Node == node() -> erlang:send({RegName, Node}, Msg);
	true -> sewer_send({RegName, Node}, Msg)
    end;

send(Pid, Msg) when is_pid(Pid) ->
    if
	node(Pid) == node() -> erlang:send(Pid, Msg);
	true -> sewer_send(Pid, Msg)
    end.
	     
connect_node(NodeName) ->
    Ref = make_ref(),
    gen_server:cast(?MODULE, {connect, NodeName, self(), Ref}),
    receive
	{ Ref, ok } -> true;
	{ Ref, _Result } -> false
    end.

nodes() -> nodes([visible]).
nodes(this) -> node();
nodes([this]) -> node();
nodes([Arg1 | ArgsTail]) ->
    Ref = make_ref(),
    gen_server:cast(?MODULE, {nodes, [Arg1 | ArgsTail], self(), Ref}),
    receive
	{ Ref, Result } -> Result
    end;
nodes(Arg1) -> nodes([Arg1]).

spawn(Fun) ->
    erlang:spawn(Fun).

spawn(Node, Fun) ->
    spawn_opt(Node, erlang, apply, [Fun, []], []).

spawn(Module, Function, Args) ->
    erlang:spawn(Module, Function, Args).

spawn(Node, Module, Function, Args) ->
    spawn_opt(Node, Module, Function, Args, []).

spawn_link(Fun) ->
    erlang:spawn_link(Fun).

spawn_link(Node, Fun) ->
    spawn_opt(Node, erlang, apply, [Fun, []], [link]).

spawn_link(Module, Function, Args) ->
    erlang:spawn_link(Module, Function, Args).

spawn_link(Node, Module, Function, Args) ->
    spawn_opt(Node, Module, Function, Args, [link]).

spawn_opt(Node, Module, Function, Args, Options) ->
    Ref = make_ref(),
    sewer_send({sewer, Node}, {remote_spawn_opt, self(), Ref,
			       Module, Function, Args, Options}),
    receive
	{ Ref, Result } -> Result
    end.    

link(PidOrPort) ->
    if
	node(PidOrPort) == node() -> erlang:link(PidOrPort);
	true -> gen_server:call(?MODULE, {link, PidOrPort})
    end.

stop() ->
    gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Options) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).
start_link() ->
    start_link([]).

start(Options) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Options, []).
start() ->
    start([]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
get_release_dir() ->
    case filename:find_src(sewer) of
	{error, {Reason, Module}} -> 
	    {error, {Reason, Module}};
	{SourcePath, _Options} ->
	    DirName = filename:dirname(SourcePath),
	    filename:join([DirName, "Release"])
    end.

get_exe_path(Options) ->
    proplists:get_value(binpath, Options,
			filename:join(get_release_dir(), "\\sewer.exe")).

get_dll_path(Options) ->
    proplists:get_value(binpath, Options, get_release_dir()).

open_sewer_port(Options) ->
    case proplists:get_value(dll, Options) of
	undefined -> open_exe_port(Options);
	_  -> open_dll_port(Options)
    end.

open_exe_port(Options) ->
    open_port({spawn_executable, get_exe_path(Options)},
	      [{packet, 4}, binary, overlapped_io]).

open_dll_port(Options) ->
    case erl_ddll:load_driver(get_dll_path(Options), "sewerlib") of
	ok -> ok;
	{error, already_loaded} -> ok;
        {error, X} -> exit({error, X});
	_ -> exit({error, could_not_load_driver})
    end,
    open_port({spawn_driver, "sewerlib"},
	      [stream, binary]).

init(Options) ->
    case is_alive() of 
	true -> 
	    P = open_sewer_port(Options),
	    port_send(P, {init, self(), Options}),
	    receive
		{P, {data, Binary}} ->
		    case binary_to_term(Binary) of
			ok ->
			    process_flag(trap_exit, true),
			    {ok, #state{port=P, links=dict:new()}};
			{error, Reason} ->
			    {stop, Reason};
			BadResponse ->
			    {stop, io_lib:format("Unrecognized response from "
						 "port: ~p~n.", [BadResponse])}
		    end
	    after
		50000 -> {stop, timeout}
	    end;
	false ->
	    {stop, "You must first run net_kernel:start() to give "
	     "this node a name."}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({link, RemotePid}, LocalPid, State) ->
    erlang:link(LocalPid),
    sewer_send({sewer, node(RemotePid)}, {remote_link, RemotePid, LocalPid},
	      State#state.port),
    {reply, true, append_link(LocalPid, RemotePid, State)};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({connect, NodeName, From, Ref}, State) ->
    port_send(State#state.port, {connect, NodeName, From, Ref}),
    {noreply, State};

handle_cast({send, BinMsg}, State) ->
    port_command(State#state.port, BinMsg),
    {noreply, State};

handle_cast({nodes, Args, From, Ref}, State) ->
    port_send(State#state.port, {nodes, erlang:node(), Args, From, Ref}),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {data, Binary}}, State)
  when is_port(Port); is_binary(Binary) ->
    Term = binary_to_term(Binary),
    case Term of
	% Send is a special case with a concatenated header and payload.
	% Term will only contain the header.  Extract the payload.
	{send, Dest, ByteSize} -> 
	    MsgPart = binary_part(Binary, byte_size(Binary) - ByteSize,
				  ByteSize),
	    erlang:send(Dest, binary_to_term(MsgPart)),
	    {noreply, State};
	_ ->
	    handle_port(Term, State)
    end;

handle_info({remote_spawn_opt, FromPid, Ref, Module, Function, Args, Options},
	    State) ->
    ShouldLink = proplists:get_bool(link, Options),
    Spawned = erlang:spawn_opt(Module, Function, Args, Options),
    NewState = 
	if
	    is_pid(Spawned) andalso ShouldLink ->
		append_link(Spawned, FromPid, State);
	    true -> State
	end,
    sewer_send({sewer, node(FromPid)},
	       {remote_spawned, FromPid, Ref, Spawned, ShouldLink},
	       State#state.port),
    {noreply, NewState};

handle_info({'EXIT', Pid, Reason}, State) ->
    NewState = 
	case dict:find(Pid, State#state.links) of
	    {ok, Pids} ->
		SewerProcs = get_node_procs(Pids),
		Msg = {remote_exit, Pid, Reason},
		Port = State#state.port,
		lists:foreach(fun(Proc) -> sewer_send(Proc, Msg, Port) end,
			      SewerProcs),
		NewLinks = dict:erase(Pid, State#state.links),
		State#state{links=NewLinks};
	    error ->
		State
	end,
    {noreply, NewState};

handle_info({remote_exit, Pid, Reason}, State) ->
    NewState =
	case dict:find(Pid, State#state.links) of
	    {ok, Pids} ->
		lists:foreach(fun(Proc) -> exit(Proc, Reason) end,
			      lists_unique(Pids)),
		NewLinks = dict:erase(Pid, State#state.links),
		State#state{links=NewLinks};
	    error ->
		State
	end,
    {noreply, NewState};

handle_info({remote_link, LocalPid, RemotePid}, State) ->
    NewState = 
	case erlang:link(LocalPid) of
	    noproc ->
		sewer_send({sewer, node(RemotePid)},
			   {remote_exit, LocalPid, noproc},
			   State#state.port),
		State;
	    _ ->
		append_link(LocalPid, RemotePid, State)
	end,
    {noreply, NewState}; 
	 
handle_info({remote_spawned, From, Ref, Result, ShouldLink}, State) ->
    NewState = 
	if is_pid(Result) andalso ShouldLink ->
		append_link(Result, From, State);
	   true ->
		State
	end,
    erlang:send(From, {Ref, Result}),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    port_close(State#state.port),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling port messages
%%
%% @spec handle_port(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_port({call_completed, Pid, Ref, Result}, State) ->
    Pid ! {Ref, Result},
    {noreply, State};

handle_port({pipe_error, PipeName, ReadOrWrite, BytesToTransfer,
	     BytesTransferred, ErrorCode, ErrorText, SourceLine}, State) ->
    io:format("Error ~w while ~tsing pipe ~s: ~tsTransferred ~w of ~w~n"
	      "Source line: ~w~n",
	      [ErrorCode, ReadOrWrite, PipeName, ErrorText, BytesTransferred,
	       BytesToTransfer, SourceLine]),
    {noreply, State}.

%% Other internal functions.

pack_send(Dest, Msg) ->
    Payload = term_to_binary(Msg),
    Header = term_to_binary({send, Dest, byte_size(Payload)}),
    list_to_binary([Header, Payload]).

sewer_send(Dest, Msg) ->
    gen_server:cast(?MODULE, {send, pack_send(Dest, Msg)}),
    Msg.

sewer_send(Dest, Msg, SewerPort) ->
    port_command(SewerPort, pack_send(Dest, Msg)),
    Msg.

% Given a list of pids, returns a list of
% [{sewer, Node1}, {sewer, Node2}, ...]
% with any duplicates removed.
get_node_procs(Pids) ->
    lists_unique(lists:map(fun(P) -> {sewer, node(P)} end, Pids)).

lists_unique(List) ->
    Set = ordsets:from_list(List),
    ordsets:to_list(Set).

append_link(FromPid, ToPid, State) ->
    Links1 = dict:append(FromPid, ToPid, State#state.links),
    Links2 = dict:append(ToPid, FromPid, Links1),
    State#state{links=Links2}.

port_send(Port, Msg) ->
    port_command(Port, term_to_binary(Msg)),
    Msg.
	   


