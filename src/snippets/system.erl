%% list of all loaded modules in erlang
code:all_loaded().
io:fwrite("~p", [code:all_loaded()]).   %% Some verbose level output...

%% List of registered processes in the erlang node
io:fwrite("~p", [ registered() ]).
%% Get the pid of a registered module
whereis(mycoolmodulename).
% <0.123456.7>
%% Exit/kill a process (when possible)
exit(Pid, Reason).
% exit(Pid, normal).    %% Normal mode
% exit(Pid, shutdown).  %%
% exit(Pid, kill).      %% The hard way

%% Pid 'o' Rama
Pid = list_to_pid("<0.37.0>").
% Pid = <0.37.0>
pid_to_list(Pid).
% "<0.37.0>"


%% Check if a function is defined: true|false (http://erlang.org/doc/man/erlang.html#function_exported-3)
erlang:function_exported(io, fwrite, 2).
%% Sample:
%% if erlang:function_exported(ejabberd_sql_pt, sql__mark, 1) =:= true ->
%%     io:fwrite("Defined", []);
%% true ->
%%     io:fwrite("NotDefined", [])
%% end.
%%
%% ...but even better:
%% (credits: gootik @slack channel)
is_defined({M, F, A}) ->
    code:load_file(M),
    erlang:function_exported(M, F, A).

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
