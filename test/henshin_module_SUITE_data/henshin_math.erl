-module(henshin_math).
-compile({parse_transform, henshin_module}).

add(XAST, YAST) :-
    X <- XAST,
    Y <- YAST,
    erl_parse:abstract(X + Y).
