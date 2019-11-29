-module(deeperl_usage_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_HEADER,     [
    {<<"content-type">>, "application/x-www-form-urlencoded; charset=utf-8"},
    {<<"user-agent">>, "deeperl/0.1.0"}
]).

request_test() ->
    {Uri, Header, Body} = gen_deeperl_procedure:request(deeperl_usage, "authkey", []),
    [
        ?assertEqual("/v2/usage", Uri),
        ?assertEqual(?DEFAULT_HEADER, Header),
        ?assertEqual([["auth_key=", "authkey"]], Body)
    ].

response_test() ->
    [
        ?assertEqual(
            {180118, 1250000},
            gen_deeperl_procedure:response(deeperl_usage, <<"{\"character_count\": 180118, \"character_limit\": 1250000}">>)
        )
    ].
