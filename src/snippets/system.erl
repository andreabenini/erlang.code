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

inet:i().
%% list of all ports opened and which processes own them, example:
%%
%% Port  Module   Recv     Sent    Owner        Local Address              Foreign Address         State     Type   
%% 2169  inet_tcp 131422   216295  <0.13265.25> localhost:42139            localhost:57576         CONNECTED STREAM 
%% 3310  inet_tcp 1914415  335858  <0.1998.14>  2.127.220.124:33948        2.118.109.157:postgres  CONNECTED STREAM 
%% 4185  inet_tcp 123449   105472  <0.2791.27>  2.127.220.124:xmpp-client  107.123.122.137:53063   CONNECTED STREAM 
%% 5134  inet_tcp 0        0       <0.1772.14>  localhost:websm            *:*                     ACCEPTING STREAM 
%% 6718  inet_tcp 267      132     <0.1901.14>  2.127.220.124:43842        15.112.123.64:http      CONNECTED STREAM 
%% ...

flush().
%% Flush incoming unread messages (console or module or whatever)

l(http_uri).
%% Load http_uri on console so autocomplete feature works for this new module too
