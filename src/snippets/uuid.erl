%% coding: UTF-8
%%
-module(uuid).
-export([uuid/0]).

% Returns a string representation of a binary UUID.
uuid() ->
    % Random binary UUID.
    RandomString = uuid(crypto:rand_uniform(1, round(math:pow(2, 48))) - 1,
                        crypto:rand_uniform(1, round(math:pow(2, 12))) - 1,
                        crypto:rand_uniform(1, round(math:pow(2, 32))) - 1,
                        crypto:rand_uniform(1, round(math:pow(2, 30))) - 1),
    %% Flatten up to a mere string/list
    lists:flatten(
        io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
                      uuidParts(RandomString))
    ).
uuid(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.


% Returns the 32, 16, 16, 8, 8, 48 parts of a binary UUID.
uuidParts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].
