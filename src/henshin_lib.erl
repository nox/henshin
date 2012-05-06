-module(henshin_lib).
-compile({no_auto_import, [error/2]}).

%% Interface

-export([marker/4]).

%% Implementation

-spec marker(error | warning, module(), term(), erl_syntax:syntaxTree()) ->
	erl_syntax:syntaxTree().
marker(Type, Mod, Reason, Node) ->
	Line = erl_syntax:get_pos(Node),
	case Type of
		error -> erl_syntax:error_marker({Line, Mod, Reason});
		warning -> erl_syntax:warning_marker({Line, Mod, Reason})
	end.
