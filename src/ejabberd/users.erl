%% Getting user last activity
mod_last:get_last_info(<<"ben">>, <<"whatever.com">>).
% {ok,1497632173,<<>>}      %% UNIX Timestamp representing last login
mod_last:get_last_info(<<"wtf">>, <<"whatever.com">>).
% not_found                 %% ATOM (user never logged in)
