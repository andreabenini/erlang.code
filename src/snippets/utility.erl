
%% useful defines
-define(FUN_NAME,  element(2, element(2, process_info(self(), current_function)))).     %% Return current function name

%% Just show us where the errors are...
-define(PRINT_STACK_TRACE, erlang:get_stacktrace()).

%% ERROR GET LINE - Returns the line where the error has been generated
%% @param FunctionName (atom) Name of the function to search in the stack trace [mostly ?FUN_NAME]
%% @return (binary) Error line number as binary type
errorGetLine(FunctionName) ->
    StackFunctionError = lists:keysearch(FunctionName, 2, erlang:get_stacktrace()),
    {value, {_, _, _, LineInfo} } = StackFunctionError,                         %% {value,{mod_restapi,chatSend,3, [{file,"src/mod_restapi.erl"},{line,1203}]}}
    {value, {line, LineError}} = lists:keysearch(line, 1, LineInfo),            %% {value,{line,1203}}
    integer_to_binary(LineError).
-define(ERROR_LINE, errorGetLine(?FUN_NAME)).
%% Basic usage:
io:fwrite("Error [L.~s == ~s]", [?ERROR_LINE, errorGetLine(?FUN_NAME)]),
