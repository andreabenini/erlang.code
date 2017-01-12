
%% useful defines
-define(FUN_NAME,  element(2, element(2, process_info(self(), current_function)))).     %% Return current function name

%% Just show us where the errors are...
-define(PRINT_STACK_TRACE, erlang:get_stacktrace()).

%% list of all loaded modules in erlang
code:all_loaded()

%% Check if a function is defined: true|false (http://erlang.org/doc/man/erlang.html#function_exported-3)
erlang:function_exported(io, fwrite, 2).
%% ...even better:
%% (credits: gootik @slack channel)
is_defined({M, F, A}) ->
    code:load_file(M),
    erlang:function_exported(M, F, A).
