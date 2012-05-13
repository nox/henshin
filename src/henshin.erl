-module(henshin).

%% Interface

-export([format_error/1, parse_transform/2]).

-type error() :: defined_rule | {ill_defined_rules, module()} |
    {rule_badreturn, {{module(), {atom(), byte()}}, term()}} |
    {rule_error, {module(), {atom(), byte()}}, term()} |
    {undefined_module, module()}.

-export_type([error/0]).

-type rules() :: [{module(), [{atom(), byte()}]}].

%% Implementation

-spec format_error(error()) -> io_lib:chars().
format_error(defined_rule) ->
    "henshin rules cannot be defined when applying them";
format_error({ill_defined_rules, M}) ->
    io_lib:format("module ~w rules list erroneously defined", [M]);
format_error({rule_badreturn, {{M, {F, A}}, Return}}) ->
    io_lib:format("bad return value from rule ~w:~w/~w: ~p", [M, F, A, Return]);
format_error({rule_error, {M, {F, A}}, Reason}) ->
    io_lib:format("error in rule ~w:~w/~w: ~p", [M, F, A, Reason]);
format_error({undefined_module, M}) ->
    io_lib:format("module ~w undefined", [M]).

-spec parse_transform(erl_syntax:forms(), [compile:option()]) ->
        erl_syntax:forms().
parse_transform(Forms, _CompileOpts) ->
    erl_syntax:revert_forms(apply_rules(Forms)).

%% Internal

-spec apply_rules(erl_syntax:forms()) -> erl_syntax:forms().
apply_rules(Forms) ->
    {Result, _} = lists:mapfoldl(
        fun (Form, Rules) ->
            case erl_syntax:type(Form) of
                function ->
                    apply_rules(
                        erl_syntax_lib:annotate_bindings(Form, ordsets:new()),
                        Rules);
                rule ->
                    Marker = henshin_lib:marker(
                        error, ?MODULE, defined_rule, Form),
                    {[Marker], Rules};
                _ ->
                    {[Form], Rules}
            end
        end, [], Forms),
    lists:flatten(Result).

-spec apply_rules(erl_syntax:syntaxTree(), rules()) ->
    {erl_syntax:forms(), rules()}.
apply_rules(Fun, Rules) ->
    {Result, {Rules2, Markers}} = erl_syntax_lib:mapfold(
        fun apply_rules_node/2, {Rules, []}, Fun),
    {[Result | Markers], Rules2}.

-spec apply_rules_node(
    erl_syntax:syntaxTree(), {rules(), erl_syntax:forms()}) ->
        {erl_syntax:syntaxTree(), {rules(), erl_syntax:forms()}}.
apply_rules_node(Node, Acc) ->
    case erl_syntax:type(Node) of
        application ->
            apply_rules_app(Node, Acc);
        _ ->
            {Node, Acc}
    end.

-spec apply_rules_app(
    erl_syntax:syntaxTree(), {rules(), erl_syntax:forms()}) ->
        {erl_syntax:syntaxTree(), {rules(), erl_syntax:forms()}}.
apply_rules_app(App, Acc = {Rules, Markers}) ->
    case erl_syntax_lib:analyze_application(App) of
        MFA = {M, {_F, _A}} ->
            case rule_exported(MFA, Rules) of
                {true, Rules2} ->
                    apply_rule(App, {Rules2, Markers}, MFA);
                {false, Rules2} ->
                    {App, {Rules2, Markers}};
                {ill_defined_rules, Rules2} ->
                    Marker = henshin_lib:marker(
                        error, ?MODULE, {ill_defined_rules, M}, App),
                    {App, {Rules2, [Marker | Markers]}};
                {undefined_module, Rules2} ->
                    Marker = henshin_lib:marker(
                        warning, ?MODULE, {undefined_module, M}, App),
                    {App, {Rules2, [Marker | Markers]}}
            end;
        _ ->
            % @todo handle imported rules
            {App, Acc}
    end.

-spec rule_exported({module(), {atom(), byte()}}, rules()) ->
    {true, rules()} | {false, rules()}
        | {ill_defined_rules, rules()} | {undefined_module, rules()}.
rule_exported(MFA = {M, FA}, Rules) ->
    case lists:keyfind(M, 1, Rules) of
        {M, FAs} ->
            lists:member(FA, FAs);
        false ->
            case code:ensure_loaded(M) of
                {module, M} ->
                    call_henshin_rules(MFA, Rules);
                {error, _Reason} ->
                    {undefined_module, [{M, []} | Rules]}
            end
    end.

-spec call_henshin_rules({module(), {atom(), byte()}}, rules()) ->
    {true, rules()} | {false, rules()} | {ill_defined_rules, rules()}.
call_henshin_rules({M, FA}, Rules) ->
    case erlang:function_exported(M, henshin_rules, 0) of
        true ->
            case catch M:henshin_rules() of
                FAs when is_list(FAs) ->
                    Valid = lists:all(
                        fun ({F, A})
                            when is_atom(F), is_integer(A), A >= 0, A =< 255 ->
                                true;
                            (_) ->
                                false
                        end, FAs),
                    case Valid of
                        true ->
                            {lists:member(FA, FAs), [{M, FAs} | Rules]};
                        false ->
                            {ill_defined_rules, [{M, []} | Rules]}
                    end;
                _ ->
                    {ill_defined_rules, [{M, []} | Rules]}
            end;
        false ->
            {false, [{M, []} | Rules]}
    end.

-spec apply_rule(
    erl_syntax:syntaxTree(), {rules(), erl_syntax:forms()},
            {module(), {atom(), byte()}}) ->
        {erl_syntax:syntaxTree(), {rules(), erl_syntax:forms()}}.
apply_rule(App, Acc = {Rules, Markers}, MFA = {M, {F, _A}}) ->
    case catch apply(M, F, erl_syntax:application_arguments(App)) of
        {'EXIT', Reason} ->
            Marker = henshin_lib:marker(
                error, ?MODULE, {rule_error, MFA, Reason}, App),
            {App, {Rules, [Marker | Markers]}};
        Result ->
            case is_expr(Result) of
                true ->
                    apply_rules_node(Result, Acc);
                false ->
                    Marker = henshin_lib:marker(
                        error, ?MODULE, {rule_badreturn, {MFA, Result}}, App),
                    {App, {Rules, [Marker | Markers]}}
            end
    end.

-spec is_expr(_) -> boolean().
is_expr(Term) ->
    case catch erl_lint:exprs([erl_syntax:revert(Term)], []) of
        {'EXIT', _} -> false;
        _ -> true
    end.
