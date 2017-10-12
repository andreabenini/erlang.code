%% Show available checkpoints in the VM
mnesia:system_info(checkpoints).
% -> [list,of,checkpoints,here]

%% Show current backup modules (filesystem is the most common)
mnesia:system_info(backup_module).
% -> mnesia_backup

%% Count records in a table
(ejabberd@localhost)81> mnesia:table_info('table1', size).
% -> 9


%% Backup of a single table with a checkpoint
tableBackup(TableName, FileName) ->
    mnesia:activate_checkpoint([{name, TableName},
                                {min,  [TableName]},
                                {ram_overrides_dump, true} ]),  %% Even if it's a ram only table
    mnesia:backup_checkpoint(TableName, binary_to_list(FileName)),
    mnesia:deactivate_checkpoint(TableName).
%% Example:
tableBackup(nameOfTheTable, <<"/tmp/backup">>).
%% -> ok

%% Restore a table from the backup file
tableRestore(TableName, FileName) ->
    mnesia:restore(binary_to_list(FileName), [{clear_tables, [TableName]} ]).
%% Example:
tableRestore(nameOfTheTable, <<"/tmp/backup">>).
%% -> ok.
