%% spawn/1 -> spawn(fun() -> server("Hello") end). 
%% spawn/3 -> spawn(moduleName, funName, ["argument", "list"]),  %% ?MODULE

%%
%% Hello world spawn
-module(helloworld). 
-export([start/0]). 
start() ->
   spawn(fun() -> server("Hello") end). 
server(Message) ->
   io:fwrite("~p",[Message]).

%%
%% A little bit complex (but not that much)
-module(spawn). 
-export([loop/0,start/0]). 
loop() ->
   receive 
      {rectangle, Width, Ht} -> 
         io:fwrite("Area of rectangle is ~p~n" ,[Width * Ht]), 
         loop(); 
      {circle, R} ->
      io:fwrite("Area of circle is ~p~n" , [3.14159 * R * R]), 
      loop(); 
   _Other ->
      io:fwrite("Unknown"), 
      loop() 
   end. 
start() ->
   Pid = spawn(fun() -> loop() end), 
   Pid ! {rectangle, 6, 10}.
