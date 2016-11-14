%% TABLE CREATE - Create a table with disk storage
%% @param TableName  (atom)    Name of the table
%% @param RecordType (record)  -define record(...)
%% @param RecordInfo (record_info type) record_info type, or its define
%% @param TableType  (atom)    set:unique, bag:multiple
%% @param RecordKey  (index pkey) primary key or table index
%%
%% @return ok|notinitialized (atom) operation result
tableCreate(TableName, RecordType, RecordInfo, TableType, RecordKey) ->
    case lists:member(TableName, mnesia:table_info(schema, tables)) of
        true ->
            io:fwrite("Table '~p' already exists, its creation is not necessary", [TableName]);
        _ ->
            case mnesia:create_table(TableName, [
                                          {disc_copies, [node()]},
                                          {record_name, RecordType},
                                          {attributes,  RecordInfo},
                                          {index,       [RecordKey]},
                                          {type,        TableType}
                                          ] ) of
                {atomic, ok} ->
                    io:fwrite("table '~p' created", [TableName]);
                Error ->
                    io:fwrite("table '~p' creation failed (~p)", [TableName, ?PRINT_VAR(Error)]),
                    notInitialized
            end
    end.
%% Record Example
-record(tableRecord, {
            filename = [],
            pattern  = [],
            template = []
        }).
%% Statement invoke sample
%% record_info cannot be redefined and must be passed or hardcoded into create_table attribs
tableCreate(tableName, tableRecord, record_info(fields, tableRecord), bag, #tableRecord.pattern),

    
%% TABLE RECORD UPDATE - Insert/Update a record in the table
tableRecordUpdate(TableName, Record) ->
    TheRecord = fun() -> mnesia:write(TableName, Record, write) end,
    mnesia:transaction(TheRecord).
%% ...and a simple call of it
modulename:recordUpdate(myTableName, #tableRecord{
                                                   filename = FileName,
                                                   pattern  = MyPattern,
                                                   template = Template  }),


%% TABLE RECORD DELETE - Delete records from table
%% @param TableName (atom)    Table name
%% @param Condition (#record) Selection criteria based on table record
%% @return ok (atom) Nothing returned
%%
%% @see   Selection criteria example : #record{field="something_to_del", _='_'}
tableRecordDelete(TableName, Condition) ->
    F = fun() ->
        DeleteCriteria = mnesia:match_object(TableName, Condition, read),
        lists:foreach(fun(Element)->
                            mnesia:delete_object(TableName, Element, write)
                      end,
                      DeleteCriteria)
    end,
    mnesia:transaction(F).


%% here's the call:
modulename:tableRecordDelete(tablename, #record{fieldname="conditionValueToDelete", _='_'}).


%% TABLE RECORD SELECT - Select a record in the table
%% @param TableName (atom) Table name
%% @param Criteria  (#record) Selection criteria based on table record
%% @return Result   (#record) SELECT results
tableRecordSelect(TableName, Criteria) ->
    F = fun() -> mnesia:match_object(TableName, Criteria, read) end,
    mnesia:transaction(F).

%% ...and, here's the call:
tableRecordSelect(sessiona, #session{sessionid=console, _='_'}).

%% SELECT all records from a table with an iterator
%% '[]' is where the accumulator might put its initial (and successive) value
showTableWithIterator(TableName)->
    Iterator =  fun(Record, _)->
                    io:format("~p~n",[Record]),
                    []
                end,
    case mnesia:is_transaction() of
        true ->
            mnesia:foldl(Iterator, [], TableName);
        false -> 
            Exec = fun({Function,Table}) -> mnesia:foldl(Function, [], Table) end,
            mnesia:activity(transaction, Exec, [{Iterator,TableName}], mnesia_frag)
    end.
%% ...and here's the call
showTableWithIterator(nameOfMyCoolTable).


%% TABLE DELETE - Delete a table from a mnesia schema
%% @param  TableName (atom) The table to delete
%% @return (atom) {aborted, Reason} | {atomic, ok}
tableDelete(TableName) ->
    mnesia:delete_table(TableName).


%% SETTING DEFAULT - Set default value if it doesn't exist a value for Variable
%% @param TableName    (atom) Name of the table where the setting should be stored
%% @param Variable     (list) Variable name, detect if it exists in TableName
%% @param DefaultValue (list) Default value to assign if Variable doesn't exists
settingDefault(TableName, Variable, DefaultValue) ->
    try
        F = fun() -> mnesia:match_object(TableName, #setting{variable=botname, _='_'}, read) end,
        {atomic, [_SomeValue]} = mnesia:transaction(F)      %% Something exists, I don't even care about its result but I'm ok
    catch _:_ ->
        io:fwrite("~p doesn't have a value, default set to ~p~n", [Variable, DefaultValue]),
        mnesia:transaction(fun() ->
                                Record = #setting{variable=Variable, value=DefaultValue},
                                mnesia:write(TableName, Record, write)
                           end)
    end.
%% ... and its call:
%% @see: table "setting" type is set (not bag)
settingDefault(TableSetting, variable, "My Default Value").
