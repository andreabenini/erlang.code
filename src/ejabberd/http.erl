-define(HTTP_REQUEST_TIMEOUT, 30000).
-define(PROFILE, myOwnProfile).

request(Domain) ->
    URL = "http://www.google.com",      %% URL must be a string, not a binary stream
    %% HTTP Request
    case httpc:request(get, {URL, []}, [{timeout, ?HTTP_REQUEST_TIMEOUT}], [], ?PROFILE) of
        %% OK: Server replied with OK [HTTP: 200..29]
        {ok, {{_HTTPType, ReturnCode, _State}, Headers, BodyMessage}} when ReturnCode >= 200, ReturnCode =< 299 ->
            io:format("HTTP Reply  ~p, Headers ~p", [ReturnCode, Headers]),
            io:format("[HTTP:~p] ~p", [ReturnCode, BodyMessage]);

        %% ERROR: Server replied with an error
        {ok, {{_HTTPType, ReturnCode, _State}, Headers, BodyMessage}} when ReturnCode < 200; ReturnCode > 299 ->
            io:format("ERROR: [HTTP:~p] Headers ~p", [ReturnCode, Headers]),
            io:format("ERROR: Body       ~p", [BodyMessage]);

        %% ERROR: Something else is wrong
        Error ->
            io:format("ERROR: requesting ~s, Error = ~p", [URL, Error])
    end.
