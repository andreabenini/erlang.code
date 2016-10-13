%% coding: UTF-8
%%
%% @file    server.erl - Just a simple gen_server implementation, use it as a foundation for your projects
%% @date    (2016/10)
%% @author  Andrea Benini
%%
%% @history v1.0.0 (2016/10)
%%              First working version, no error handling (proof of concept)
%%              - Basic code taken from OTP, erlang.org and few #slack advices
%%          v1.0.1 (2016/10)
%%              - Lots of comments to improve readability
%%              - Pretty output and dummy func*() for debugging/tracing
%%              - Tests with Sync vs Async calls (we're living in a parallel world, isn't it ?)
%%
-module(server).
-author('Andrea Benini <andreabenini at google domain gmail.com>').

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
    start/0,                            %% Start server
    start/1,                            %%
    start_link/1,                       %% Overloading gen_server:start_link/4
    stop/0                              %% Stop server
]).

-export([                               %% Module functions exports
    func1/0,                            %% func1() custom function
    func2/0,                            %% func2() custom function
    func2/1,                            %%
    func3/0                             %% func3() custom function
]).


%% @see gen_server behaviour
%% -------------------------------------------------------------------

%% START - Starting server and linking into it
start() ->
    start([]).
start(Arguments) ->
    io:format("start      ) Starting server. Arguments=~p~n", [Arguments]),
    start_link(Arguments).

%% START LINK - Execute operations before linking to the new server
start_link(Arguments) ->
    io:fwrite("start_link ) Starting server. Arguments=~p~n", [Arguments]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Arguments], []).

%% STOP - Stopping the server (async)
stop() ->
    io:fwrite("stop       ) Stopping server~n", []),
    gen_server:cast(?MODULE, stop).

%% INIT - Callback used after linking to the newly created server
init(Arguments) ->
    io:fwrite("init server) Initializing server ~p with ~p~n", [?MODULE, Arguments]),
    process_flag(trap_exit, true),    %% Receive a message if my child process terminates (normally or not)
    {ok, Arguments}.

%% TERMINATE - Terminate server
terminate(Reason, State) ->
    io:fwrite("terminate  ) Reason=~p, State=~p~n", [Reason, State]),
    ok.

%% HANDLE CALL - Synchronous call
%%      {reply,Reply,NewState}
%%      {reply,Reply,NewState,Timeout}
%%      {reply,Reply,NewState,hibernate}
%%      {noreply,NewState}
%%      {noreply,NewState,Timeout}
%%      {noreply,NewState,hibernate}
%%      {stop,Reason,Reply,NewState}
%%      {stop,Reason,NewState}
handle_call({atomIdForSomething, Parameters, OtherParameters}, From, State) ->
    io:fwrite("handle_call) [atomIdForSomething,~p,~p] From=~p, State=~p~n", [Parameters, OtherParameters, From, State]),
    {reply, ok, State};
handle_call(mycustomatom, From, State) ->
    io:fwrite("handle_call) [mycustomatom] From=~p, State=~p~n", [From, State]),
    % gen_server:call(?SERVER, {atomIdForSomething, [firstParam, secondParam], otherParams}),   %% Don't do that ! Timeout error (handle_call is sync !)
    {reply, ok, State};
handle_call(DontKnow, From, State) ->
    io:fwrite("handle_call) handle_call=~p, From=~p, State=~p~n", [DontKnow, From, State]),
    {reply, error, State}.

%% HANDLE CAST - Asynchronous call
%%      {noreply,NewState}
%%      {noreply,NewState,Timeout}
%%      {noreply,NewState,hibernate}
%%      {stop,Reason,NewState} normal termination clause
handle_cast(stop, State) ->
    io:fwrite("handle_cast) stop, State=~p~n", [State]),
    {stop, normal, State};
handle_cast(asyncCall1, State) ->
    io:fwrite("handle_cast) [asyncCall1], here goes State=~p~n", [State]),
    {noreply, State};
handle_cast(asyncCall2, State) ->
    io:fwrite("handle_cast) [asyncCall2], here goes State=~p~n", [State]),
    gen_server:cast(?MODULE, asyncCall1),
    {noreply, State};
handle_cast(Message, State) ->
    io:fwrite("handle_cast) Message=~p, State=~p~n", [Message, State]),
    {noreply, State}.

%% HANDLE INFO - Informative call
%%      {noreply,NewState}
%%      {noreply,NewState,Timeout}
%%      {noreply,NewState,hibernate}
%%      {stop,Reason,NewState}
handle_info(Message, State) ->
    io:fwrite("handle_info) Message=~p, State=~p~n", [Message, State]),
    {noreply, State}.


%% CODE CHANGE - Change server code (hot loading)
code_change(OldVersion, State, Extra) ->
    io:fwrite("code_change) OldVersion=~p, State=~p, Extra=~p~n", [OldVersion, State, Extra]),
    {ok, State}.


%% @see module behaviour (exports)
%% -------------------------------------------------------------------

func1() ->
    io:fwrite("func1      ) [SYNC] Executing func1/0~n", []),
    gen_server:call(?MODULE, mycustomatom).

func2() ->
    func2([]).
func2(Params) ->
    io:fwrite("func2      ) [SYNC] Executing func2/1~n", []),
    gen_server:call(?MODULE, {atomIdForSomething, Params, otherParams}).

func3() ->
    io:fwrite("func3      ) [ASYNC] Executing func3/0~n", []),
    gen_server:cast(?MODULE, asyncCall2).


%% @see module behaviour (internal functions)
%% -------------------------------------------------------------------
