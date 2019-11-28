-module(deeperl_languages_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_HEADER,     [
    {<<"content-type">>, "application/x-www-form-urlencoded"},
    {<<"user-agent">>, "deeperl/0.1.0"}
]).

request_test() ->
    {Uri, Header, Body} = gen_deeperl_procedure:request(deeperl_languages, "authkey", []),
    [
        ?assertEqual(Uri, "/v2/languages"),
        ?assertEqual(Header, ?DEFAULT_HEADER),
        ?assertEqual(Body, "auth_key=authkey")
    ].

response_test() ->
    [
        ?assertEqual(
            [{de, "Deutsch"}, {en, "English"}, {es, "Español"}, {fr, "Français"}, {it, "Italiano"}, {nl, "Nederlands"}, {pl, "Polski"}, {pt, "Português"}, {ru, "русскийязык"}],
            gen_deeperl_procedure:response(deeperl_languages, unicode:characters_to_binary("[{\"language\":\"DE\",\"name\":\"Deutsch\"},{\"language\":\"EN\",\"name\":\"English\"},{\"language\":\"ES\",\"name\":\"Español\"},{\"language\":\"FR\",\"name\":\"Français\"},{\"language\":\"IT\",\"name\":\"Italiano\"},{\"language\":\"NL\",\"name\":\"Nederlands\"},{\"language\":\"PL\",\"name\":\"Polski\"},{\"language\":\"PT\",\"name\":\"Português\"},{\"language\":\"RU\",\"name\":\"русскийязык\"}]"))
        ),
        ?assertEqual(
            {error, {invalid_response, unicode:characters_to_binary("[{\"languge\":\"DE\",\"name\":\"Deutsch\"},{\"language\":\"EN\",\"name\":\"English\"},{\"language\":\"ES\",\"name\":\"Español\"},{\"language\":\"FR\",\"name\":\"Français\"},{\"language\":\"IT\",\"name\":\"Italiano\"},{\"language\":\"NL\",\"name\":\"Nederlands\"},{\"language\":\"PL\",\"name\":\"Polski\"},{\"language\":\"PT\",\"name\":\"Português\"},{\"language\":\"RU\",\"name\":\"русскийязык\"}]")}},
            gen_deeperl_procedure:response(deeperl_languages, unicode:characters_to_binary("[{\"languge\":\"DE\",\"name\":\"Deutsch\"},{\"language\":\"EN\",\"name\":\"English\"},{\"language\":\"ES\",\"name\":\"Español\"},{\"language\":\"FR\",\"name\":\"Français\"},{\"language\":\"IT\",\"name\":\"Italiano\"},{\"language\":\"NL\",\"name\":\"Nederlands\"},{\"language\":\"PL\",\"name\":\"Polski\"},{\"language\":\"PT\",\"name\":\"Português\"},{\"language\":\"RU\",\"name\":\"русскийязык\"}]"))
        ),
        ?assertEqual(
            {error, {invalid_json, unicode:characters_to_binary("[{\"languge\"\"DE\",\"name\":\"Deutsch\"},{\"language\":\"EN\",\"name\":\"English\"},{\"language\":\"ES\",\"name\":\"Español\"},{\"language\":\"FR\",\"name\":\"Français\"},{\"language\":\"IT\",\"name\":\"Italiano\"},{\"language\":\"NL\",\"name\":\"Nederlands\"},{\"language\":\"PL\",\"name\":\"Polski\"},{\"language\":\"PT\",\"name\":\"Português\"},{\"language\":\"RU\",\"name\":\"русскийязык\"}]")}},
            gen_deeperl_procedure:response(deeperl_languages, unicode:characters_to_binary("[{\"languge\"\"DE\",\"name\":\"Deutsch\"},{\"language\":\"EN\",\"name\":\"English\"},{\"language\":\"ES\",\"name\":\"Español\"},{\"language\":\"FR\",\"name\":\"Français\"},{\"language\":\"IT\",\"name\":\"Italiano\"},{\"language\":\"NL\",\"name\":\"Nederlands\"},{\"language\":\"PL\",\"name\":\"Polski\"},{\"language\":\"PT\",\"name\":\"Português\"},{\"language\":\"RU\",\"name\":\"русскийязык\"}]"))
        )
    ].
