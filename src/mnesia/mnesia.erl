%% TABLE CREATE - Create a table with disk storage
tableCreate(TableName, RecordType, RecordInfo, RecordKey) ->
    case lists:member(TableName, mnesia:table_info(schema, tables)) of
        true ->
            io:fwrite("Table '~p' already exists, its creation is not necessary", [TableName]);
        _ ->
            case mnesia:create_table(TableName, [
                                          {disc_copies, [node()]},
                                          {record_name, RecordType},
                                          {attributes,  RecordInfo},
                                          {index,       [RecordKey]},
                                          {type,        bag}                %% set:unique, bag:multiple
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
tableCreate(tableName, tableRecord, record_info(fields, tableRecord), #tableRecord.pattern),

    
%% TABLE RECORD UPDATE - Insert/Update a record in the table
tableRecordUpdate(TableName, Record) ->
    TheRecord = fun() -> mnesia:write(TableName, Record, write) end,
    mnesia:transaction(TheRecord).
%% ...and a simple call of it
programERdb:recordUpdate(myTableName, #tableRecord{
                                                   filename = FileName,
                                                   pattern  = MyPattern,
                                                   template = Template  }),


%% Verify this if it works !
tableRecordDelete(TableName, Condition) ->
    F = fun() ->
            DeleteCriteria = mnesia:match_object(TableName, Condition, read),
            lists:foreach(fun(Element)->
                              io:format("TableName=~p, Element=~p", [TableName, Element]),
                              mnesia:delete_object(Element)
                          end, DeleteCriteria)
    end,
    mnesia:activity(async_dirty, F).
