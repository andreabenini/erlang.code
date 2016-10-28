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
