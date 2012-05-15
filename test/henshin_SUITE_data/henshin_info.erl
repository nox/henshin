-module(henshin_info).
-compile({parse_transform, henshin}).
-export([trivia/1]).

trivia(answer) ->
	henshin_math:add(39, 3).
