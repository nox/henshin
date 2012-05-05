-module(henshin_lib).
-compile({no_auto_import, [error/2]}).

%% Interface

-export([error/2, warning/2, transform/2]).

-type module_forms() :: [erl_parse:abstract_form(), ...].
-type pass() :: fun((module_forms()) -> {continue | stop, module_forms()}).

-export_type([module_forms/0, pass/0]).

%% Implementation

-spec error({module(), term()}, non_neg_integer()) -> erl_parse:abstract_form().
error({Mod, Reason}, Line) ->
    erl_syntax:revert(erl_syntax:error_marker({Line, Mod, Reason})).

-spec warning({module(), term()}, non_neg_integer()) ->
	erl_parse:abstract_form().
warning({Mod, Reason}, Line) ->
    erl_syntax:revert(erl_syntax:warning_marker({Line, Mod, Reason})).

-spec transform([pass()], module_forms()) -> module_forms().
transform([Pass | Passes], Forms) ->
    case Pass(Forms) of
        {continue, NewForms} -> transform(Passes, NewForms);
        {stop, NewForms} -> NewForms
    end;
transform([], Forms) ->
    Forms.
