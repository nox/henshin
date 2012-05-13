-module(bgen).
-compile({parse_transform, henshin_module}).
-include("empty_include.hrl").

illegal_rule(Arg) :-
    <<>> <= Arg.
