%% Dealing with XML messages
Message =
    {xmlel,<<"message">>,
        [{<<"xml:lang">>,<<"en">>},
         {<<"type">>,<<"chat">>},
         {<<"to">>,<<"test2@domain.com">>},
         {<<"id">>,<<"aadea">>}],
        [{xmlcdata,<<"\n">>},
         {xmlel,<<"body">>,[],[{xmlcdata,<<"The Body Message">>}]},
         {xmlcdata,<<"\n">>},
         {xmlel,<<"active">>,
                [{<<"xmlns">>,<<"http://jabber.org/protocol/chatstates">>}],
                []},
         {xmlcdata,<<"\n">>}]}.
%% Data extraction
fxml:get_subtag(Message, <<"body">>).                       %% {xmlel,<<"body">>,[],[{xmlcdata,<<"The Body Message">>}]}
fxml:get_tag_cdata(fxml:get_subtag(Message, <<"body">>)).   %% <<"The Body Message">>

%% Looking for a specific namespace
Packet = 
    {xmlel,<<"iq">>,
       [{<<"type">>, <<"set">>},
        {<<"id">>, <<"e4e717d3-d1f8-4a37-9109-af1b448dbb75:sendIQ">>}],
       [{xmlel,<<"query">>,
               [{<<"xmlns">>,<<"jabber:iq:roster">>}],
               [{xmlel,<<"item">>, [{<<"jid">>,<<"ben@whatever.com">>},{<<"name">>,<<"ben@whatever.com">>}], []}]}
       ]
    }.
fxml:get_subtag_with_xmlns(Packet, <<"query">>, <<"jabber:iq:roster">>).
%% FOUND     -> {xmlel,<<"query">>,[{<<"xmlns">>,<<"jabber:iq:roster">>}],[{xmlel,<<"item">>,[{<<"jid">>,<<"ben@whatever.com">>},{<<"name">>,<<"ben@whatever.com">>}],[]}]}
%% NOT FOUND -> false


%% Getting content from Query
Query = 
    {xmlel,<<"query">>,
       [{<<"xmlns">>,<<"jabber:iq:roster">>}],
       [{xmlel,<<"item">>,
               [{<<"jid">>,<<"ben@whatever.com">>},
                {<<"name">>,<<"ben@whatever.com">>}],
               []}
       ]
    }.
QueryData = fxml:get_subtag(Query, <<"item">>),
%% #xmlel.children = {xmlel, <<"item">>, [{<<"jid">>,<<"ben@whatever.com">>},{<<"name">>,<<"ben@whatever.com">>}],   []}
fxml:get_tag_attr_s(<<"jid">>, QueryData).
%% getting specific attribute value: <<"ben@whatever.com">>


%% XML to tuple
XML = fxml_stream:parse_element(<<"<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Don't forget me this weekend!</body>
</note>">>).
%% #xmlel{name = <<"note">>,attrs = [],
%%        children = [
%%            {xmlcdata,<<"\n">>},
%%            #xmlel{name = <<"to">>,attrs = [],children = [{xmlcdata,<<"Tove">>}]},{xmlcdata,<<"\n">>},
%%            #xmlel{name = <<"from">>,attrs = [],children = [{xmlcdata,<<"Jani">>}]},{xmlcdata,<<"\n">>},
%%            #xmlel{name = <<"heading">>,attrs = [],children = [{xmlcdata,<<"Reminder">>}]},{xmlcdata,<<"\n">>},
%%            #xmlel{name = <<"body">>,attrs = [],children = [{xmlcdata,<<"Don't forget me this weekend!">>}]},{xmlcdata,<<"\n">>}
%%        ]}

%% Tuple to XML
io:fwrite("~p", [fxml:element_to_binary(XML)]).
%% <<"<note>\n<to>Tove</to>\n<from>Jani</from>\n<heading>Reminder</heading>\n<body>Don&apos;t forget me this weekend!</body>\n</note>">>

%% Get PATH content data
%% VCard = {xmlel,
%%          <<"vCard">>,
%%          [{<<"xmlns">>,<<"vcard-temp">>}],
%%          [{xmlcdata,<<"\n">>},
%%           {xmlel,<<"FN">>,[],[{xmlcdata,<<"Ben">>}]},
%%           {xmlcdata,<<"\n">>},
%%           {xmlel,<<"PHOTO">>,[],[{xmlcdata,<<"\n">>},
%%                                  {xmlel,<<"TYPE">>,[],[{xmlcdata,<<"image/png">>}]},
%%                                  {xmlcdata,<<"\n">>},
%%                                  {xmlel,<<"BINVAL">>,[],[{xmlcdata,<<"\uuencodesomevalue">>}]},
%%                                  ...]},...]}
io:fwrite("Image Content Type = ~p", [ fxml:get_path_s(VCard, [{elem, <<"PHOTO">>}, {elem, <<"TYPE">>}, cdata]) ]).
%% Image Content Type = <<"image/png">>
