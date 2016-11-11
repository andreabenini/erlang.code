
%% useful defines
-define(FUN_NAME,  element(2, element(2, process_info(self(), current_function)))).     %% Return current function name

%% list of all loaded modules in erlang
code:all_loaded()
