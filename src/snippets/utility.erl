
%% useful defines
-define(FUN_NAME,  element(2, element(2, process_info(self(), current_function)))).     %% Return current function name

%% Just show us where the errors are...
-define(PRINT_STACK_TRACE, erlang:get_stacktrace()).

%% list of all loaded modules in erlang
code:all_loaded()

%% Check if a function is defined: true|false (http://erlang.org/doc/man/erlang.html#function_exported-3)
erlang:function_exported(io, fwrite, 2).
%% ...even better:
%% (credits: gootik @slack channel)
is_defined({M, F, A}) ->
    code:load_file(M),
    erlang:function_exported(M, F, A).

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
%% @see: do NOT use "erlang:now()", "erlang:timestamp()" is preferred
unixTimestamp() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),     %% Timestamp
    MegaSecs * 1000000 + Secs.
