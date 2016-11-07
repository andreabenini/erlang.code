%% TABLE CREATE - Create a table with disk storage
%% @param TableName  (atom)    Name of the table
%% @param RecordType (record)  -define record(...)
%% @param RecordInfo (record_info type) record_info type, or its define
%% @param TableType  (atom)    set:unique, bag:multiple
%% @param RecordKey  (index pkey) primary key or table index
%%
%% @return ok|notinitialized (atom) operation result
tableCreate(TableName, RecordType, TableType, RecordInfo, RecordKey) ->
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


%% TABLE DELETE - Delete a table from a mnesia schema
%% @param  TableName (atom) The table to delete
%% @return (atom) {aborted, Reason} | {atomic, ok}
tableDelete(TableName) ->
    mnesia:delete_table(TableName).
