%% Apple push notification service sample file
%%
%% Certfile with KEY+CERT
%% apns:send("My coolest message", <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>, "/path/to/file/apple.pem", "pemPassword").
%% Certfiles with separate KEY and CERT
%% apns:send("Another cool message", <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>, "/path/to/file/apple.crt.pem", "/path/to/file/apple.key.pem", "pemPassword").
%%
-module(apns).
-define(APPLE_SERVER_HOST,      "gateway.push.apple.com").          %% Produzione URL
% -define(APPLE_SERVER_HOST, "gateway.sandbox.push.apple.com").       %% Development URL
-define(APPLE_SERVER_PORT,      2195).
-define(APPLE_SERVER_TIMEOUT,   20000).                             %% Timeout: 20s

-export([
    send/4,
    send/5
]).

send(Message, Token, FileCertificatePEM, Password) ->
    %% Crypto, SSL
    crypto:start(),
    ssl:start(),
    %% Connection parameters
    Options = case Password of
        undefined ->
            [{certfile, FileCertificatePEM}, {mode, binary}]; %, {verify, verify_none}
        _ ->
            [{certfile, FileCertificatePEM}, {password, Password}, {mode, binary}]
    end,
    io:fwrite("Options = ~p~n", [Options]),
    sendPush(Message, Token, Options).
send(Message, Token, FileCertificate, FileKey, Password) ->
    %% Crypto, SSL
    crypto:start(),
    ssl:start(),
    %% Connection parameters
    Options = case Password of
        undefined ->
            [{certfile, FileCertificate}, {keyfile, FileKey}, {mode, binary}]; %, {verify, verify_none}
        _ ->
            [{certfile, FileCertificate}, {keyfile, FileKey}, {password, Password}, {mode, binary}]
    end,
    io:fwrite("Options = ~p~n", [Options]),
    sendPush(Message, Token, Options).


sendPush(Message, Token, Options) ->
    io:fwrite("Connecting to ~s:~p...~n", [?APPLE_SERVER_HOST, ?APPLE_SERVER_PORT]),
    case ssl:connect(?APPLE_SERVER_HOST, ?APPLE_SERVER_PORT, Options, ?APPLE_SERVER_TIMEOUT) of
        {ok, Socket} ->
            %% Just for sample purposes, a message must be jsonized better with the proper library (Jiffy, ...)
            MessageJSON     = io_lib:fwrite("{\"aps\":{\"alert\":\"~s\",\"sound\":\"default\"}}", [Message]),
            io:fwrite("JSON Message=~s~n", [MessageJSON]),
            %% Formatting proper vars
            MessageBin      = list_to_binary(MessageJSON),
            MessageBinLen   = size(MessageBin),
            TokenNumeric    = erlang:binary_to_integer(Token, 16),
            TokenBinary     = <<TokenNumeric:32/integer-unit:8>>,
            %% Building and sending packet
            Packet          = <<
                    0:8,
                    32:16/big,
                    TokenBinary/binary,
                    MessageBinLen:16/big,
                    MessageBin/binary
            >>,
            ssl:send(Socket, Packet),
            ssl:close(Socket),
            io:fwrite("Push notification sent~n", []);
        {error, Reason} ->
            io:fwrite("SSL connection failed: ~p~n", [Reason])
    end,
    io:fwrite("Program terminated~n", []).
