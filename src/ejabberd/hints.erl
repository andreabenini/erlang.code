%% List of online rooms (for every domain in the current host)
ets:tab2list(muc_online_room).

%% Getting conference host from XMPP domain
%% @param Host (binary) the XMPP domain. ie: <<"domain.com">>
%% @return ConferenceHost (binary) Something like: <<"conference.domain.com">>
getConferenceHost(Host) ->
    gen_mod:get_module_opt_host(Host, mod_muc, <<"conference.@HOST@">>).
