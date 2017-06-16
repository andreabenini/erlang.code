%% On init set the proper hook
init([Host, Opts]) ->
    %% ...
    %% filter_packet URL must be called with global flag set
    ejabberd_hooks:add(filter_packet, global, ?MODULE, filterSomething, 50),
    %% user_send_packet get raised for each single packet sent from an user
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, myownMessageHandler, 50),
    %% ...
    
    %% Dealing with handlers
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1, one_queue),
    %% Beware of well known namespaces, I have seen weird behavior when dealing with ?NS_ROSTER for example
    %% Use it when it's strictly necessary and deal with your own NS only
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?MYOWN_NS, ?MODULE, iqHandler, IQDisc),
    
    %% ...

terminate(_Reason, _State) ->
    %% ...
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, filterSomething, 50),

    %% It's nice to have the XMPP Domain in _State as a module variable
    ejabberd_hooks:delete(user_send_packet, XMPPDomain, ?MODULE, myownMessageHandler, 50),
    %% ...
    
    %% Removing handlers
    gen_iq_handler:remove_iq_handler(ejabberd_sm, XMPPDomain, ?MYOWN_NS),
    %% ...

filterSomething(Packet) ->
    ?INFO_MSG("Packet is ~p", [Packet]),
    Packet.

vcardMessageHandler(Packet, C2SState, Source, Destination) ->
    ?INFO_MSG("Source=~p, Destination=~p, State=~p, Packet=~p", [Source, Destination, C2SState, Packet]),
    Packet.

iqHandler(From, To, IQ) ->
    ?CRITICAL_MSG("From=~p, To=~p, IQ=~p", [From, To, IQ]),
    IQ.
