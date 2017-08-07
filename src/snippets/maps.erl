%% Get Key from map value
%%
-define(ACTIONS, #{received  => 10, displayed => 20}).

getStatusFromValue(SearchValue) when is_binary(SearchValue)->
    getStatusFromValue(string:str(  maps:values(?ACTIONS),[list_to_integer(binary_to_list(SearchValue))]  ));
getStatusFromValue(Pos) when Pos>0 ->
    atom_to_binary(lists:nth(Pos, maps:keys(?ACTIONS)), utf8);
getStatusFromValue(0) ->
    <<"notfound">>.

%% Usage Example
getStatusFromValue(10).
% <<"received">>
getStatusFromValue(20).
% <<"displayed">>
