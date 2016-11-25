%% Quick and useful commands available on debug console
%%

%% Brief command help
help().

%% information about the running system
i().

%% Killing in the name of... <0.760.0>
exit(pid(0,760,0), kill).


%% Huge process info items
%% Just initial call
[io:format("~p : ~p~n",[Pid,erlang:process_info(Pid,initial_call)]) || Pid <- processes()].
%% Current executing function...
[io:format("~p : ~p~n",[Pid,erlang:process_info(Pid,current_function)]) || Pid <- processes()].
%% huuuge info on all processes...
[io:format("~p : ~p~n",[Pid,erlang:process_info(Pid)]) || Pid <- processes()].
%% Sample Item Record-->
%%<5629.3454.0> : [{current_function,{gen,do_call,4}},
%%                 {initial_call,{proc_lib,init_p,5}},
%%                 {status,waiting},
%%                 {message_queue_len,0},
%%                 {messages,[]},
%%                 {links,[#Port<5629.33684>]},
%%                 {dictionary,
%%                     [{'$ancestors', [<5629.628.0>,ejabberd_listeners,ejabberd_sup,<5629.38.0>]},
%%                      {'$initial_call',{ejabberd_http,init,2}}]},
%%                 {trap_exit,false},
%%                 {error_handler,error_handler},
%%                 {priority,normal},
%%                 {group_leader,<5629.37.0>},
%%                 {total_heap_size,2962},
%%                 {heap_size,2586},
%%                 {stack_size,88},
%%                 {reductions,1109},
%%                 {garbage_collection,
%%                     [{min_bin_vheap_size,46422},
%%                      {min_heap_size,233},
%%                      {fullsweep_after,65535},
%%                      {minor_gcs,6}]},
%%                 {suspending,[]}]