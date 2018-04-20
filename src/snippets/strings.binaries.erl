%% Lists stuff
> AA = io_lib:format("Hello World, result is ~p", [42]).
[72,101,108,108,111,32,87,111,114,108,100,44,32,114,101,115,117,108,116,32,105,115,32,"42"]
> lists:flatten(AA).
"Hello World, result is 42"
> lists:concat([helloAtom," World, result is ", 42, "\n"]).
"helloAtom World, result is 42\n"

%% Some binaries
erlang:iolist_to_binary(AA).
% <<"Hello World, result is 42">>
<<"Hi there, ", (list_to_binary(AA))/binary>>,
% <<"Hi there, Hello World, result is 42">>

%% Trim string  (left | right | both)
string:strip(SomeStringData, both).
%% Trim binary string
list_to_binary( string:strip(binary_to_list(SomeBinaryData), both) ).
