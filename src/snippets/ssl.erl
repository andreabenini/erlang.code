%% Sample not properly working, taken from slack as a referral for future tests

-module(ssl).
-export([start/0, client/1, accept/1, socket_to_issuer_id/1]).

start() ->
   ssl:start(),
   server(4000).

server(Port) ->
    {ok, LSocket} = ssl:listen(Port, [
                                     {cacertfile, "ca.crt"},
                                     {verify, verify_peer},
                                     {certfile,"server.crt"},
                                     {keyfile, "server.key"},
                                     {reuseaddr, true},
                                     {active, false},
                                     {fail_if_no_peer_cert, true}
                                     ]),
    spawn(fun() -> accept(LSocket) end).
    
accept(LSocket) ->
   {ok, Socket} = ssl:transport_accept(LSocket),
   ok = ssl:ssl_accept(Socket),
   socket_to_issuer_id(Socket),
   Pid = spawn(fun() ->
        io:format("Connection accepted ~p~n", [Socket]),
        loop(Socket)
   end),
   ssl:controlling_process(Socket, Pid),
   accept(LSocket).

loop(Socket) ->
   ssl:setopts(Socket, [{active, once}]),
   receive
   {ssl,Sock, Data} ->
        io:format("Got packet: ~p~n", [Data]),
        ssl:send(Sock, Data),
        loop(Socket);
   {ssl_closed, Sock} ->
        io:format("Closing socket: ~p~n", [Sock]);
   Error ->
        io:format("Error on socket: ~p~n", [Error])
   end.

client(N) ->
    {ok, Socket} = ssl:connect("localhost", 4000,  [{cacertfile, "ca.crt"}, {verify,verify_peer},{server_name_indication,"localhost"},{depth,3}]),
    io:format("Client opened socket: ~p~n",[Socket]),
    ok = ssl:send(Socket, N),
    Value = receive
            {ssl,{sslsocket,new_ssl,_}, Data} ->
                io:format("Client received: ~p~n",[Data])
            after 2000 ->
                0
            end,
    ssl:close(Socket),
    Value.

socket_to_issuer_id(Socket) ->
        case ssl:peercert(Socket) of
                {error, no_peercert} ->
                        io:format("no peercert: ~p~n", [Socket]),
                        false;
                {ok, Cert} ->
                        {ok, IssuerID} = public_key:pkix_issuer_id(Cert, self),
                        IssuerID
        end.



%% Generating the ca, server, and client certs:
% HOST="localhost"
% DAYS=3650
% PASS="foobar"
% SUBJ="/C=US/ST=Maryland/L=Ellicott City/O=WLMS/OU=dev-test/CN=$HOST"
% 
% # Generate self-signed server and client certificates
% ## generate CA
% openssl req -new -x509 -keyout ca.key -out ca.crt -days $DAYS -nodes -subj "$SUBJ"
% 
% ## generate server certificate request
% openssl req -newkey rsa:2048 -sha256 -keyout server.key -out server.csr -days $DAYS -nodes -subj "$SUBJ"
% 
% ## sign server certificate
% openssl x509 -req -CA ca.crt -CAkey ca.key -in server.csr -out server.crt -days $DAYS -CAcreateserial
% 
% ## generate client certificate request
% openssl req -newkey rsa:2048 -sha256 -keyout client.key -out client.csr -days $DAYS -nodes -subj "$SUBJ"
% 
% ## sign client certificate
% openssl x509 -req -CA ca.crt -CAkey ca.key -in client.csr -out client.crt -days $DAYS -CAserial ca.srl
% 
% # Convert self-signed server certificate to PKCS#12 format
% openssl pkcs12 -export -name $HOST -in server.crt -inkey server.key -out server.p12 -CAfile ca.crt -passout pass:$PASS
% 
% # Convert self-signed client certificate to PKCS#12 format
% openssl pkcs12 -export -name $HOST -in client.crt -inkey client.key -out client.p12 -CAfile ca.crt -passout pass:$PASS
%%
