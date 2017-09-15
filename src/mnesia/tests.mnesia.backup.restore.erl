
%%% BACKUP/RESTORE HISTORY TEST                                 %%%
%%% JUST MERE TESTS HERE, CODE NEED TO BE SORTED AND ORGANIZED  %%%


(ejabberd@localhost)81> mnesia:table_info('table1', size).
9
(ejabberd@localhost)82> mnesia:table_info('table2', size).
2
(ejabberd@localhost)83> mnesia:table_info('table3', size).
5
(ejabberd@localhost)84> h().
64: mnesia:table_info('table4', size)
-> 0
66: mnesia:table_info('table3', size)
-> 5
67: mnesia:table_info('table1', size)
-> 9
68: mnesia:table_info('table5', size)
-> 0
69: mnesia:activate_checkpoint([{name,backupCheckpoint}, {max,
                                 ['table1',
                                  'table2',
                                  'table3']}])
-> {ok,backupCheckpoint,[ejabberd@localhost]}
70: Name = backupCheckpoint, Nodes = [ejabberd@localhost]
-> [ejabberd@localhost]
71: Name
-> backupCheckpoint
74: mnesia:system_info(checkpoints)
-> [backupCheckpoint]
75: mnesia:system_info(backup_module)
-> mnesia_backup
76: mnesia:backup_checkpoint(Name, "/tmp/tableBackup")
-> ok
77: mnesia:deactivate_checkpoint(Name)
-> ok
78: mnesia:system_info(checkpoints)
-> []
79: mnesia:table_info('table1'),
    mnesia:table_info('table2'),
    mnesia:table_info('table3')
-> {'EXIT',{undef,[{mnesia,table_info,
                           ['table1'],
                           []},
                   {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,674}]},
                   {shell,exprs,7,[{file,"shell.erl"},{line,686}]},
                   {shell,eval_exprs,7,[{file,"shell.erl"},{line,641}]},
                   {shell,eval_loop,3,[{file,"shell.erl"},{line,626}]}]}}
80: mnesia:table_info('table1')
-> {'EXIT',{undef,[{mnesia,table_info,
                           ['table1'],
                           []},
                   {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,674}]},
                   {shell,exprs,7,[{file,"shell.erl"},{line,686}]},
                   {shell,eval_exprs,7,[{file,"shell.erl"},{line,641}]},
                   {shell,eval_loop,3,[{file,"shell.erl"},{line,626}]}]}}
mnesia:table_info('table1', size).
mnesia:table_info('table2', size).
mnesia:table_info('table3', size).
