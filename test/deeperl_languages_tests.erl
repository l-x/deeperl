-module(deeperl_languages_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_HEADER,     [
    {<<"content-type">>, "application/x-www-form-urlencoded; charset=utf-8"},
    {<<"user-agent">>, "deeperl/0.1.0"}
]).

request_test() ->
    {Uri, Header, Body} = gen_deeperl_procedure:request(deeperl_languages, "authkey", []),
    [
        ?assertEqual("/v2/languages", Uri),
        ?assertEqual(?DEFAULT_HEADER, Header),
        ?assertEqual([["auth_key=", "authkey"]], Body)
    ].

response_test() ->
    [
        ?assertEqual(
            [{en,<<"English">>}, {de,<<"German">>}, {fr,<<"French">>}, {es,<<"Spanish">>}, {pt,<<"Portuguese">>}, {it,<<"Italian">>}, {nl,<<"Dutch">>}, {pl,<<"Polish">>}, {ru,<<"Russian">>}],
            gen_deeperl_procedure:response(deeperl_languages, <<"[{\"language\":\"EN\",\"name\":\"English\"},{\"language\":\"DE\",\"name\":\"German\"},{\"language\":\"FR\",\"name\":\"French\"},{\"language\":\"ES\",\"name\":\"Spanish\"},{\"language\":\"PT\",\"name\":\"Portuguese\"},{\"language\":\"IT\",\"name\":\"Italian\"},{\"language\":\"NL\",\"name\":\"Dutch\"},{\"language\":\"PL\",\"name\":\"Polish\"},{\"language\":\"RU\",\"name\":\"Russian\"}]">>)
        )
    ].
