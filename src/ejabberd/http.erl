

request(Domain) ->
    URL = "http://www.google.com",      %% URL must be a string, not a binary stream
    %% HTTP Request
    case httpc:request(get, {URL, []}, [{timeout, ?HTTP_REQUEST_TIMEOUT}], [], httpcClientName(Domain)) of
        %% OK: Server replied with OK [HTTP: 200..29]
        {ok, {{_HTTPType, ReturnCode, _State}, Headers, BodyMessage}} when ReturnCode >= 200, ReturnCode =< 299 ->
            ?TRACE("HTTP Reply  ~p, Headers ~p", [ReturnCode, Headers]),
            ?INFO_MSG("[HTTP:~p] ~p", [ReturnCode, BodyMessage]);

        %% ERROR: Server replied with an error
        {ok, {{_HTTPType, ReturnCode, _State}, Headers, BodyMessage}} when ReturnCode < 200; ReturnCode > 299 ->
            ?ERROR_MSG("[HTTP:~p] Headers ~p", [ReturnCode, Headers]),
            ?ERROR_MSG("Body       ~p", [BodyMessage]);

        %% ERRORE: Non mi ha risposto o c'è altro, lo scrivo sul log
        Error ->
            ?ERROR_MSG("ERROR requesting ~s, Error = ~p", [URL, Errore])
    end.
