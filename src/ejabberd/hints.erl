%% List of online rooms (for every domain in the current host)
ets:tab2list(muc_online_room).

%% Getting conference host from XMPP domain
%% @param Host (binary) the XMPP domain. ie: <<"domain.com">>
%% @return ConferenceHost (binary) Something like: <<"conference.domain.com">>
getConferenceHost(Host) ->
    gen_mod:get_module_opt_host(Host, mod_muc, <<"conference.@HOST@">>).

%% Get database module used in the muc system
%% @return (atom) Something like: mod_muc_sql
%% @see    it's the module that handles the db directly (mnesia, sql, riak, ..)
dbModuleForMUC(Domain) ->
    XMPPDomain = jid:nameprep(Domain),
    gen_mod:db_mod(XMPPDomain, mod_muc).
