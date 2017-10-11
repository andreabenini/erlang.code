%% Return a unix timestamp from a datetime string
%% @param DateTime (binary) A binary datetime (example from PostgreSQL: <<"2017-01-09 12:31:48.250951">>)
%% @return (integer) a valid unix datetime
bindateToTimestamp(DateTime) ->
    DateStr = binary_to_list(DateTime),                     %% Field  : "2017-01-09 12:31:48.250951"
    List    = [{1,4},{6,7},{9,10},{12,13},{15,16},{18,19}], %% Format : YYYY,MM,DD,HH,mm,SS        (Field Format)
    [Year,Month,Day,Hour,Minute,Second] = [list_to_integer(string:sub_string(DateStr, X, Y)) || {X,Y} <- List],
    UNIXTimeStamp = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}) -
                    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    UNIXTimeStamp.

%% Calculate current timestamp
%% @see: please use "erlang:now()" instead of "erlang:timestamp()"
unixTimestamp() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),     %% Timestamp
    MegaSecs * 1000000 + Secs.

%% Even simpler: unix timestamp (seconds)
erlang:system_time(second).
1501112223
%% ... or milliseconds
erlang:system_time(millisecond).
1501112223444

%% Print current datetime in an human friendly format
printDateTime() ->
    {{YY,MM,DD}, {HH,II,SS}} = calendar:local_time(),
    io_lib:format("~4..0b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b", [YY, MM, DD, HH, II, SS]).
%% io:fwrite("DateTime:~s", [printDateTime()]).            %% to call it
