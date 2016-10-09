%%
%%

%% LOG INIT - Logging facility setup
logInit() ->
    LogFile = programER:dirWork() ++ "/log/programER.log",
    filelib:ensure_dir(LogFile).
