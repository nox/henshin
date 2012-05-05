-module(henshin_lib).
-compile({no_auto_import, [error/2]}).

%% Interface

-export([error/2, warning/2]).

%% Implementation

-spec error({module(), term()}, non_neg_integer()) -> erl_parse:abstract_form().
error({Mod, Reason}, Line) ->
    erl_syntax:revert(erl_syntax:error_marker({Line, Mod, Reason})).

-spec warning({module(), term()}, non_neg_integer()) ->
	erl_parse:abstract_form().
warning({Mod, Reason}, Line) ->
    erl_syntax:revert(erl_syntax:warning_marker({Line, Mod, Reason})).
