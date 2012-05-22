-module(henshin_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).
-export([call/1, answer/1]).
-export([forbid_bgen/1, forbid_pmod/1]).

all() ->
    [{group, math}, forbid_bgen, forbid_pmod].

groups() ->
    [{math, [sequence], [call, answer]}].

call(Config) ->
    compile_and_load(henshin_math, Config),
    [{add, 2}] = henshin_math:henshin_rules(),
    ThirtyNine = erl_parse:abstract(39),
    Three = erl_parse:abstract(3),
    FortyTwo = erl_parse:abstract(42),
    FortyTwo = henshin_math:add(ThirtyNine, Three).

answer(Config) ->
    compile_and_load(henshin_info, Config),
    42 = henshin_info:trivia(answer).

forbid_bgen(Config) ->
    File = file(bgen, Config),
    {error, [{File, [Error]}], []} = validate(File, Config),
    {6, Mod, binary_generator} = Error,
    true = io_lib:deep_char_list(Mod:format_error(binary_generator)).

forbid_pmod(Config) ->
    File = file(pmod, Config),
    {error, [{File, [Error]}], []} = validate(File, Config),
    {2, Mod, parameterized_module} = Error,
    true = io_lib:deep_char_list(Mod:format_error(parameterized_module)).

%% Internal

validate(File, Config) ->
    compile(File, Config, [strong_validation]).

compile_and_load(Mod, Config) ->
    {ok, Mod, []} = compile(file(Mod, Config), Config, []),
    load(Mod, Config).

compile(File, Config, Options) ->
    compile:file(File,
        [verbose, {outdir, ?config(priv_dir, Config)}, return | Options]).

load(Mod, Config) ->
    {module, Mod} = code:load_abs(
        lists:concat([?config(priv_dir, Config), Mod])),
    code:purge(Mod),
    ok.

file(Mod, Config) ->
    lists:concat([?config(data_dir, Config), Mod, ".erl"]).
