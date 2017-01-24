%% List of online rooms (for every domain in the current host)
ets:tab2list(muc_online_room).
%% Getting Room PID
%% @param RoomName (binary) name of the room             (ie: <<"redroom">>)
%% @param ConferenceHost (binary) Conference host domain (ie: <<"conference.domain.tld">>)
{value, #muc_online_room{pid=RoomPID}} = lists:keysearch({RoomName, ConferenceHost}, 2, ets:tab2list(muc_online_room))

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

%% SEND MESSAGE - Send a message to another user
%% @param From (binary) From user
%% @param To   (binary) To user
%% @param Body (binary) body message
%% @param URL  (binary) [optional] attached URL
%% @return void(atom)
%%
%% @see   When URL is not false the message contains an URL specified as an OOB field
%%        and its description is the message Body
sendMessage(From, To, Body, false) ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),                      %% Current Timestamp
    TimeStamp = integer_to_binary(MegaSecs * 1000000 + Secs),
    ejabberd_router:route(  jid:from_string(From),                          %% From          
                            jid:from_string(To),                            %% To
                            {xmlel, <<"message">>,                          %% Messagge to send (<message/>)
                             [{<<"type">>, <<"chat">>},                     %%      type=chat
                              {<<"from">>, From},                           %%      From
                              {<<"to">>, To},                               %%      To
                              {<<"id">>, TimeStamp}],                       %%      message ID, there must be something here
                             [{xmlel, <<"body">>, [], [{xmlcdata, Body}] }] %%      message body
                            }
    );
sendMessage(From, To, Body, URL) ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),                      %% Current Timestamp
    TimeStamp = integer_to_binary(MegaSecs * 1000000 + Secs),
    ejabberd_router:route(  jid:from_string(From),                          %% From          
                            jid:from_string(To),                            %% To
                            {xmlel, <<"message">>,                          %% Message to send (<message/>)
                             [{<<"type">>, <<"chat">>},                     %%      type=chat
                              {<<"from">>, From},                           %%      From
                              {<<"to">>, To},                               %%      To
                              {<<"id">>, TimeStamp}],                       %%      message ID, there must be something here
                             [{xmlel, <<"body">>, [], [{xmlcdata, Body}] }, %%      message body
                              {xmlel, <<"x">>,
                               [{<<"xmlns">>, <<"jabber:x:oob">>}],
                               [{xmlel, <<"url">>,  [], [{xmlcdata, URL }] },
                                {xmlel, <<"desc">>, [], [{xmlcdata, Body}] }]
                              }
                             ]
                            }
    ).
