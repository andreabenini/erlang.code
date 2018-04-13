%% Quick and useful commands available on debug console
%%

%% Brief command help
help().


%% information about the running system
i().


%% Pid'o'Rama
list_to_pid("<0.760.0>").
pid(0,760,0).
%% <0.760.0>


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


%% Print erlang version from console
erlang:system_info(otp_release).


%% Show used memory
erlang:memory().
%% Sample:
%% [{total,132302616},
%%   {processes,93408000},
%%   {processes_used,93407696},
%%   {system,38894616},
%%   {atom,984217},
%%   {atom_used,960212},
%%   {binary,5709480},
%%   {code,23620774},
%%   {ets,3675192}]
%% erlang:memory() reports memory in bytes.
%% Linux way from the outside: # ps -C beam.smp -o rss
%%                             RSS
%%                             23720
%%                             24672
%%                             127528
%%                             24740


%% List of available applications in the VM
application:which_applications().
%% [{ejabberd,"ejabberd","xx.xx"},
%%  {p1_pgsql,"PostgreSQL driver","x.x.x"},
%%  {inets,"INETS  CXC xxx xx","x.x"},
%%  {mnesia,"MNESIA  CXC xxx xx","x.xx.xx"},
%%  {cache_tab,"In-memory cache Erlang / Elixir library", "x.x.x"},
%%  {stringprep,"Fast Stringprep Erlang / Elixir implementation", "x.x.x"},
%%  ...
%%  {kernel,"ERTS  CXC 138 10","4.1.1"}]
