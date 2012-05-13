-module(henshin_module_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([bgen/1, forbid_pmod/1]).

all() ->
    [bgen, forbid_pmod].

bgen(Config) ->
    File = file(bgen, Config),
    {error, [{File, [Error]}], []} = validate(File),
    Error = {6, henshin_module, binary_generator}.

forbid_pmod(Config) ->
    File = file(pmod, Config),
    {error, [{File, [Error]}], []} = validate(File),
    Error = {2, henshin_module, parameterized_module}.

%% Internal

validate(File) ->
    compile(File, [strong_validation, return_errors]).

compile(File, Options) ->
    compile:file(File, [verbose | Options]).

file(Mod, Config) ->
    lists:concat([?config(data_dir, Config), Mod, ".erl"]).
