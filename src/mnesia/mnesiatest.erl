-module(mnesiatest).

-export([init/0, insert/0, read/1]).

-record(person,
    {name,        %% atomic, unique key
     age,         %% age
     married_to,  %% name of partner or undefined
     children }). %% list of children


init() ->
    mnesia:create_table(person,[{attributes,record_info(fields,person)}]).

insert() ->
    T = fun() ->
        X = #person{name=john,
            age=36,
            married_to=ana,
            children=[josh,kelly,samantha]
        },
        mnesia:write(X)
    end,
    mnesia:transaction(T).

read(Name) ->
    R = fun() ->
        mnesia:read(person,Name,write)
    end,
    mnesia:transaction(R).
