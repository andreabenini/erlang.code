%% LOCATION REBUILD PATH - Rebuilds a full URL exploded by http_uri:parse/1
locationRebuildPath(Schema, UserInfo, Host, Port, Path, Query) ->
    lists:concat([
                  atom_to_list(Schema), "://",
                  locationRebuildPathUserInfo(UserInfo),
                  Host, locationRebuildPathPort(Port),
                  Path, Query
                 ]).
locationRebuildPathUserInfo([])       -> "";
locationRebuildPathUserInfo(UserInfo) -> lists:concat([UserInfo, "@"]).
locationRebuildPathPort(0)    -> "";
locationRebuildPathPort([])   -> "";
locationRebuildPathPort(Port) -> lists:flatten(io_lib:format(":~p", [Port])).

%% Parse and Rebuild
{ok, {Schema, UserInfo, Host, Port, Path, Query}} = http_uri:parse("https://github.com/andreabenini/erlang.code/blob/master/src/snippets/url.erl").
locationRebuildPath(Schema, UserInfo, Host, Port, Path, Query).
