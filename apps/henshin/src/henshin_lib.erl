-module(henshin_lib).
-compile({no_auto_import, [error/2]}).

%% Interface

-export([error/2]).

%% Implementation

error({Mod, Reason}, Line) ->
    erl_syntax:revert(erl_syntax:error_marker({Line, Mod, Reason})).
