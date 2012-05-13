-module(henshin_module_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([abstract/1, bgen/1, forbid_pmod/1]).

all() ->
    [abstract, bgen, forbid_pmod].

abstract(Config) ->
    {ok, henshin_math, []} = compile(file(henshin_math, Config), Config, []),
    {module, henshin_math} = code:load_abs(
        lists:concat([?config(priv_dir, Config), henshin_math])),
    ThirtyNine = erl_parse:abstract(39),
    Three = erl_parse:abstract(3),
    FortyTwo = erl_parse:abstract(42),
    FortyTwo = henshin_math:add(ThirtyNine, Three).

bgen(Config) ->
    File = file(bgen, Config),
    {error, [{File, [Error]}], []} = validate(File, Config),
    Error = {6, henshin_module, binary_generator}.

forbid_pmod(Config) ->
    File = file(pmod, Config),
    {error, [{File, [Error]}], []} = validate(File, Config),
    Error = {2, henshin_module, parameterized_module}.

%% Internal

validate(File, Config) ->
    compile(File, Config, [strong_validation]).

compile(File, Config, Options) ->
    compile:file(File,
        [verbose, {outdir, ?config(priv_dir, Config)}, return | Options]).

file(Mod, Config) ->
    lists:concat([?config(data_dir, Config), Mod, ".erl"]).
