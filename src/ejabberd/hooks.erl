%% On init set the proper hook
init([Host, Opts]) ->
    ...
    %% filter_packet URL must be called with global flag set
    ejabberd_hooks:add(filter_packet, global, ?MODULE, filterSomething, 50),

    %% user_send_packet get raised for each single packet sent from an user
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, myownMessageHandler, 50),
    ...

terminate(_Reason, _State) ->
    ...
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, filterSomething, 50),
    
    %% It's nice to have the XMPP Domain in _State as a module variable
    ejabberd_hooks:delete(user_send_packet, XMPPDomain, ?MODULE, myownMessageHandler, 50),
    ...

filterSomething(Packet) ->
    ?INFO_MSG("Packet is ~p", [Packet]),
    Packet.

vcardMessageHandler(Packet, C2SState, Source, Destination) ->
    ?INFO_MSG("Source=~p, Destination=~p, State=~p, Packet=~p", [Source, Destination, C2SState, Packet]),
    Packet.
