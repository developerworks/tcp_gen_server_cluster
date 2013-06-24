%% Copyright
-module(test).
-author("Administrator").

%% API
-export([]).


print() ->
    io:format("print messages").


test() ->
    F = {mod_test, get_username},

