%% Example: recursive and anonymous function with an accumulator
%%          it generates an URL encoded string as result, something like: "&MyNick=ben&email=ben%40gmail.com"
QueryParameters = [{<<"mail">>, <<"ben@gmail.com">>}, {<<"nick">>, <<"ben">>}],
SearchTerms = fun(Params) ->
                  InnerSearch =
                      fun(_Fun, [], Accumulator)         ->
                              Accumulator;
                         (Fun, [Item|List], Accumulator) ->
                              {Name, Value} = Item,
                              VariableName  = case Name of
                                                  <<"nick">> -> <<"MyNick">>;
                                                  <<"mail">> -> <<"email">>;
                                                  <<"id">>   -> <<"UID">>;
                                                  Other      -> Other
                                              end,
                              Fun(Fun,
                                  List,
                                  [ lists:concat(["&",http_uri:encode(binary_to_list(VariableName)),"=",http_uri:encode(binary_to_list(Value))])
                                  ] ++ Accumulator)
                      end,
                  InnerSearch(InnerSearch, Params, "")
              end,
%% And here it goes:
io:fwrite("URL=~p" [SearchTerms(QueryParameters)]).
