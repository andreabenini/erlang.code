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

fxml:get_subtag(Message, <<"body">>).                       %% {xmlel,<<"body">>,[],[{xmlcdata,<<"The Body Message">>}]}
fxml:get_tag_cdata(fxml:get_subtag(Message, <<"body">>)).   %% <<"The Body Message">>
