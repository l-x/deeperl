-module(deeperl_usage_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_HEADER,     [
    {<<"content-type">>, "application/x-www-form-urlencoded"},
    {<<"user-agent">>, "deeperl/0.1.0"}
]).

request_test() ->
    {Uri, Header, Body} = gen_deeperl_procedure:request(deeperl_usage, "authkey", []),
    [
        ?assertEqual(Uri, "/v2/usage"),
        ?assertEqual(Header, ?DEFAULT_HEADER),
        ?assertEqual(Body, "auth_key=authkey")
    ].

response_test() ->
    [
        ?assertEqual(
            {180118, 1250000},
            gen_deeperl_procedure:response(deeperl_usage, <<"{\"character_count\": 180118, \"character_limit\": 1250000}">>)
        ),
        ?assertEqual(
            {error, {invalid_json, <<"{\"character_count\" 180118, \"character_limit\": 1250000}">>}},
            gen_deeperl_procedure:response(deeperl_usage, <<"{\"character_count\" 180118, \"character_limit\": 1250000}">>)
        ),
        ?assertEqual(
            {error, {invalid_response, <<"{\"character_cunt\": 180118, \"character_limit\": 1250000}">>}},
            gen_deeperl_procedure:response(deeperl_usage, <<"{\"character_cunt\": 180118, \"character_limit\": 1250000}">>)
        )
    ].
