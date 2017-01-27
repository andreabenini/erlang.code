%% On init set the proper hook
init([Host, Opts]) ->
    ...
    %% filter_packet URL must be called with global flag set
    ejabberd_hooks:add(filter_packet, global, ?MODULE, filterSomething, 50),
    ...

terminate(_Reason, _State) ->
    ...
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, filterSomething, 50),
    ...

filterSomething(Packet) ->
    ?INFO_MSG("Packet is ~p", [Packet]),
    Packet.
