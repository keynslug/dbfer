-module(dbfer_schema_test).
-include_lib("eunit/include/eunit.hrl").

schema_test_() ->
    [
        ?_assertMatch(_, dbfer_schema:chars("T", 20)),
        ?_assertMatch(_, dbfer_schema:logical("T")),
        ?_assertMatch(_, dbfer_schema:date("T")),
        ?_assertError({badarg, length}, dbfer_schema:chars("T", 255)),
        ?_assertError({badarg, length}, dbfer_schema:chars("T", 0)),
        ?_assertError({badarg, length}, dbfer_schema:float("T", 0, 0)),
        ?_assertError({badarg, precision}, dbfer_schema:float("T", 1, 1)),
        ?_assertError({badarg, length}, dbfer_schema:numeric("T", 21, 16)),
        ?_assertError({badarg, precision}, dbfer_schema:numeric("T", 16, 16))
    ].
