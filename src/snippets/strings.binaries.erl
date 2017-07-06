%% Lists stuff
> AA = io_lib:format("Hello World, result is ~p", [42]).
[72,101,108,108,111,32,87,111,114,108,100,44,32,114,101,115,117,108,116,32,105,115,32,"42"]
> lists:flatten(AA).
"Hello World, result is 42"
 
