%% coding: UTF-8
%%
-module(test).
-define(HTTP_REQUEST_TIMEOUT, 30000).
-define(PROFILE, myOwnProfile).

-export([                               %% gen_server behaviour
    start/0,
    request/1
]).

start() ->
    io:format("[start]~n", []),              
    inets:start(),
    Result = inets:start(httpc, [{profile, ?PROFILE}]),
    io:format("  ~p~n", [Result]).

request(URL) ->
    %% HTTP Request
    case httpc:request(get, {URL, []}, [{timeout, ?HTTP_REQUEST_TIMEOUT}], [], ?PROFILE) of
        %% OK: Server replied with OK [HTTP: 200..29]
        {ok, {{_HTTPType, ReturnCode, _State}, Headers, BodyMessage}} when ReturnCode >= 200, ReturnCode =< 299 ->
            io:format("HTTP Reply  ~p, Headers ~p~n", [ReturnCode, Headers]),
            io:format("[HTTP:~p] ~p~n", [ReturnCode, BodyMessage]);

        %% ERROR: Server replied with an error
        {ok, {{_HTTPType, ReturnCode, _State}, Headers, BodyMessage}} when ReturnCode < 200; ReturnCode > 299 ->
            io:format("ERROR: [HTTP:~p] Headers ~p~n", [ReturnCode, Headers]),
            io:format("ERROR: Body       ~p~n", [BodyMessage]);

        %% ERROR: Generic error
        Error ->
            io:format("ERROR: requesting ~s, Error = ~p~n", [URL, Error])
    end.
