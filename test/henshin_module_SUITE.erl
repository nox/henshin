-module(henshin_module_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([forbid_pmod/1]).

all() ->
    [forbid_pmod].

forbid_pmod(Config) ->
    File = file(pmod, Config),
    {error, [{File, [Error]}], []} = compile(File, [return_errors]),
    Error = {2, henshin_module, parameterized_module}.

%% Internal

compile(File, Options) ->
    compile:file(File, [verbose | Options]).

file(Mod, Config) ->
    lists:concat([?config(data_dir, Config), Mod, ".erl"]).
