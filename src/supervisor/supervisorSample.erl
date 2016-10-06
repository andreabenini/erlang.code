%% coding: UTF-8
%%
%% @file    supervisorSample.erl - Just a simple supervisor implementation, use it as a foundation for your projects
%% @date    (2016/10)
%% @author  Andrea Benini
%%
%% @history 1.0
%%          First working version
%%
%% @see     This module calls gen_server.erl worker module
%%
-module(supervisorSample).
-behaviour(supervisor).

%% API
-export([                               %% supervisor behaviour
    start_link/0
]).

-export([                               %% module function exports
    init/1
]).

-define(MODULE_ID, ?MODULE).            %% myOwnCustomAtomID  vs  ?MODULE  vs whateverElseAtomYouUse


%% @see supervisor behaviour
%% -------------------------------------------------------------------

%% start_link() -> {ok,Pid} | ignore | {error,Error}
%% @see Starts the supervisor
start_link() ->
    io:format("supervisorSample) start_link~n"),
    supervisor:start_link({local, ?MODULE_ID}, ?MODULE, []).

%% init(Args) -> {ok, {SupFlags,[ChildSpec]}} | ignore | {error, Reason}
%% @see Whenever a supervisor is started using supervisor:start_link/[2,3],
%%      this function is called by the new process to find out about restart
%%      strategy, maximum restart frequency and child specifications.
init(Params) ->
    io:format("supervisorSample) init~n"),
    ChildSpec = {                               %% Supervisor child definition, see Erlang docs
            server,                             %% ID: atom child spec for the supervisor
            {server, start_link, [Params]},     %% StartFunc: {Module, Function, Parameters}
            permanent,                          %% permanent|transient|temporary
            3000,                               %% brutal_kill|NumericValue|infinity
            worker,                             %% supervisor|worker
            [server]                            %% List of modules to call (the gen_server module itself)
    },
    {ok, {{one_for_all, 1, 1}, [ChildSpec]}}.   %% Supervisor start_link/3, see Erlang docs
