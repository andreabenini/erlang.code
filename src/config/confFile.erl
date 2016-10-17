%%
%% scrapbook ideas, nothing fancy here...
%%

%% 
logInit() ->
    LogFile = programER:dirWork() ++ "/log/myfile.log",
    filelib:ensure_dir(LogFile).
