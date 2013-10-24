-module(dbfer_encode_test).
-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [
        ?_assertEqual(<<"hill billy      ">>, dbfer_encoder:encode(dbfer_schema:chars("T", 16), "hill billy")),
        ?_assertError({badarg, _}, dbfer_encoder:encode(dbfer_schema:chars("T", 8), "hill billy")),
        ?_assertError({badarg, _}, dbfer_encoder:encode(dbfer_schema:logical("T"), "hill billy")),
        ?_assertEqual(<<"Y">>, dbfer_encoder:encode(dbfer_schema:logical("T"), true)),
        ?_assertEqual(<<"N">>, dbfer_encoder:encode(dbfer_schema:logical("T"), false)),
        ?_assertEqual(<<"20121222">>, dbfer_encoder:encode(dbfer_schema:date("T"), {{2012, 12, 22}, {0, 0, 0}})),
        ?_assertEqual(<<" -0.0001">>, dbfer_encoder:encode(dbfer_schema:numeric("T", 8, 4), -1)),
        ?_assertError({badarg, _}, dbfer_encoder:encode(dbfer_schema:numeric("T", 6, 4), -1)),
        ?_assertEqual(<<"0.1234">>, dbfer_encoder:encode(dbfer_schema:numeric("T", 6, 4), 1234)),
        ?_assertEqual(<<"-1234.5678">>, dbfer_encoder:encode(dbfer_schema:numeric("T", 10, 4), -12345678)),
        ?_assertEqual(<<"  -1234.56">>, dbfer_encoder:encode(dbfer_schema:float("T", 10, 2), -1234.5678))
    ].
