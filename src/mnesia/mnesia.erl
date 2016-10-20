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
