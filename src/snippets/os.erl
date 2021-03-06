%% Operating System operations

%% OPTION1: Execute a shell command with os:cmd
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Detects where GraphicsMagick is (REQUIRED FOR IMAGES MANIPULATIONS)
GraphicsMagickUtility = case os:cmd("which gm 2>/dev/null") of
                             [] -> throw(utilityGraphicsMagickNotFoundOnFileSystem);
                             UtilityPath -> re:replace(UtilityPath, "(^\\s+)|(\\s+$)", "", [global,{return,list}])   %% Strip CR/LF from string
                        end,
io:fwrite("GraphicsMagick = ~p)", [GraphicsMagickUtility]).


%% OPTION2: Execute a shell command with open_port
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Business logic
-define(RUN_TIMEOUT, 10000). %% in ms so it's 10s
run(Cmd) -> run(Cmd, ?RUN_TIMEOUT).
run(Cmd, Timeout) ->
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    loop(Port,[], Timeout).
loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Port, Data++NewData, Timeout);
        {Port, {exit_status, 0}} -> Data;
        {Port, {exit_status, S}} -> throw({commandfailed, S})
    after
        Timeout -> throw(timeout)
    end.
%% even Data++NewData=[NewData|Data]  but then   {Port, {exit_status, 0}} -> lists:reverse(Data);

%% OPTION3: Execute a shell command and waits errorLevel
%%    just a wrapper of open_port, an alternative to os:cmd/1
%% Hint: Adding timeout could be a nice idea (Combine option2 and option3)
cmdExec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    cmdExec_data(Port, []).
cmdExec_data(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            cmdExec_data(Port, [Sofar|Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} -> true
            end,
            receive
                {'EXIT',  Port,  _} -> ok
                after 1 -> ok            % force context switch
            end,
            ExitCode = receive
                {Port, {exit_status, Code}} -> Code
            end,
            {ExitCode, lists:flatten(Sofar)}
    end.


%% And here's some test Code
test() ->
    shouldReturnCommandResult(),
    shouldThrowAfterTimeout(),
    shouldThrowIfCmdFailed(),
    {ok, "Tests PASSED"}.
shouldReturnCommandResult() ->
    "Hello\n" = run("echo Hello").
shouldThrowAfterTimeout()->
    timeout = (catch run("sleep 10", 20)).
shouldThrowIfCmdFailed()->
    {commandfailed, _} = (catch run("wrongcommand")),
    {commandfailed, _} = (catch run("ls nonexistingfile")).
