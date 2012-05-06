Henshin!
========

From [Wikipedia][1]:
> Henshin (変身 henshin) is the Japanese word for "transformation," literally
> meaning, "to change or transform the body."

[1]: http://en.wikipedia.org/wiki/Henshin

What is it?
-----------

Henshin is a parse transform using some forgotten syntax rules of Erlang to
implement compile-time function calls, rules:

``` erlang
rule -> rule_clauses : build_rule('$1').

rule_clauses -> rule_clause : ['$1'].
rule_clauses -> rule_clause ';' rule_clauses : ['$1'|'$3'].

rule_clause -> atom clause_args clause_guard rule_body :
    {clause,?line('$1'),element(3, '$1'),'$2','$3','$4'}.

rule_body -> ':-' lc_exprs: '$2'.
```

These rules used to be used by Mnemosyne, the ancestor of QLC; you can find
some references to it in erl_lint:

``` erlang
format_error({mnemosyne, What}) ->
    "mnemosyne " ++ What ++ ", missing transformation".
```

Henshin transforms every rule to an equivalent exported function,
rejecting any binary generator and replacing any list generator with a match
and a call to `erl_syntax:concrete/1`. It also exports a new function
`Mod:henshin_rules/0` which returns the list of rules defined by the module.

Let `example` be the following module:

``` erlang
-module(example).
-compile({parse_transform, henshin_module}).

add(XAST, YAST) :-
    X <- XAST,
    Y <- YAST,
    erl_syntax:abstract(X + Y).
```

This hypothetical module would be transformed to:

``` erlang
-module(example).
-export([henshin_rules/0, add/2]).

henshin_rules() ->
    [{add, 2}].

add(XAST, YAST) ->
    X = erl_syntax:concrete(XAST),
    Y = erl_syntax:concrete(YAST),
    erl_syntax:abstract(X + Y).
```

What is there to do?
--------------------

* Write better documentation.
* Actually call the transformation rules used in modules thrown at henshin.
