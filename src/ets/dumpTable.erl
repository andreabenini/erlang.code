%% Sample code for dumping an entire ETS table
%%
dumpETSTable(TableName) ->
    FirstKey = ets:first(TableName),
    dumpETSTable(TableName, FirstKey, [FirstKey]).

dumpETSTable(_TableName, '$end_of_table', ['$end_of_table'|Acc]) ->
    Acc;
dumpETSTable(TableName, CurrentKey, Acc) ->
    NextKey = ets:next(TableName, CurrentKey),
    dumpETSTable(TableName, NextKey, [NextKey|Acc]).
