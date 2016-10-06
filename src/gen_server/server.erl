-module(server).
-author('Andrea Benini <andreabenini@gmail.com>').

-behaviour(gen_server).

-export([                               %% gen_server behaviour
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([                               %% Module custom functions
    start/1,                            %% Start server
    start_link/1,                       %% Overloading gen_server:start_link/4
    stop/0                              %% Stop server
]).

-export([                               %% Module functions exports
    myfunc1/0,                          %% my own custom function
    myfunc2/1                           %% my own custom function
]).

-define(MODULE_ID, ?MODULE).            %% myOwnCustomAtomID  vs  ?MODULE


%% @see gen_server behaviour
%% -------------------------------------------------------------------
init(Args) ->
    io:fwrite("init) Initializing module ~p with ~p~n", [?MODULE_ID, Args]),
    % process_flag(trap_exit, true),    %% Receive a message if my child process terminates (normally or not)
    {ok, Args}.

handle_call(mycustomatom, From, State) ->
    io:fwrite("call) From=~p, State=~p~n", [From, State]),
    {reply, ok, State};
handle_call(DontKnow, From, State) ->
    io:fwrite("call) handle_call=~p, From=~p, State=~p~n", [DontKnow, From, State]),
    {reply, error, State}.

handle_cast(stop, State) ->
    io:fwrite("cast) stop, State=~p~n", [State]),
    {stop, normal, State};
handle_cast(Message, State) ->
    io:fwrite("cast) Message=~p, State=~p~n", [Message, State]),
    {noreply, State}.

handle_info(Message, State) ->
    io:fwrite("info) Message=~p, State=~p~n", [Message, State]),
    {noreply, State}.

terminate(Reason, State) ->
    io:fwrite("terminate) Reason=~p, State=~p~n", [Reason, State]),
    ok.

code_change(OldVersion, State, Extra) ->
    io:fwrite("OldVersion=~p, State=~p, Extra=~p~n", [OldVersion, State, Extra]),
    {ok, State}.


%% @see module behaviour (exports)
%% -------------------------------------------------------------------
start(Parameters) ->
    io:format("start) Starting server, Parameters=~p~n", [Parameters]),
    gen_server:start_link({local, ?MODULE_ID}, ?MODULE_ID, [Parameters], []).

start_link(Parameters) ->
    io:fwrite("start_link) Starting server~n", []),
    gen_server:start_link({local, ?MODULE_ID}, ?MODULE_ID, [Parameters], []).

stop() ->
    io:fwrite("Stopping server~n", []),
    gen_server:cast(?MODULE_ID, stop).

myfunc1() ->
    gen_server:call(?MODULE, mycustomatom).

myfunc2(Params) ->
    gen_server:call(?MODULE, {atomIdForSomething, Params, otherParams}).


%% @see module behaviour (internal functions)
%% -------------------------------------------------------------------
