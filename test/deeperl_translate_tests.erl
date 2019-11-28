-module(deeperl_translate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_HEADER,     [
    {<<"content-type">>, "application/x-www-form-urlencoded"},
    {<<"user-agent">>, "deeperl/0.1.0"}
]).

request_test() ->
    assertRequest(en, ["simple text"], #{}, "text=simple%20text&target_lang=EN"),
    assertRequest(en, ["simple text", "another text"], #{}, "text=simple%20text&text=another%20text&target_lang=EN"),

    assertRequest(en, ["simple text"], #{source_lang => de}, "text=simple%20text&source_lang=DE&target_lang=EN"),

    assertRequest(en, ["simple text"], #{split_sentences => true}, "text=simple%20text&split_sentences=1&target_lang=EN"),
    assertRequest(en, ["simple text"], #{split_sentences => false}, "text=simple%20text&split_sentences=0&target_lang=EN"),
    assertRequest(en, ["simple text"], #{split_sentences => nonewlines}, "text=simple%20text&split_sentences=nonewlines&target_lang=EN"),

    assertRequest(en, ["simple text"], #{preserve_formatting => true}, "text=simple%20text&preserve_formatting=1&target_lang=EN"),
    assertRequest(en, ["simple text"], #{preserve_formatting => false}, "text=simple%20text&preserve_formatting=0&target_lang=EN"),

    assertRequest(en, ["simple text"], #{tag_handling => xml}, "text=simple%20text&tag_handling=xml&target_lang=EN"),

    assertRequest(en, ["simple text"], #{outline_detection => true}, "text=simple%20text&outline_detection=1&target_lang=EN"),
    assertRequest(en, ["simple text"], #{outline_detection => false}, "text=simple%20text&outline_detection=0&target_lang=EN"),

    assertRequest(en, ["simple text"], #{non_splitting_tags => []}, "text=simple%20text&non_splitting_tags=&target_lang=EN"),
    assertRequest(en, ["simple text"], #{non_splitting_tags => ["fu"]}, "text=simple%20text&non_splitting_tags=fu&target_lang=EN"),
    assertRequest(en, ["simple text"], #{non_splitting_tags => ["fu", "bar"]}, "text=simple%20text&non_splitting_tags=fu%2Cbar&target_lang=EN"),

    assertRequest(en, ["simple text"], #{splitting_tags => []}, "text=simple%20text&splitting_tags=&target_lang=EN"),
    assertRequest(en, ["simple text"], #{splitting_tags => ["fu"]}, "text=simple%20text&splitting_tags=fu&target_lang=EN"),
    assertRequest(en, ["simple text"], #{splitting_tags => ["fu", "bar"]}, "text=simple%20text&splitting_tags=fu%2Cbar&target_lang=EN"),

    assertRequest(ru, ["simple text"], #{ignore_tags => []}, "text=simple%20text&ignore_tags=&target_lang=RU"),
    assertRequest(ru, ["simple text"], #{ignore_tags => ["fu"]}, "text=simple%20text&ignore_tags=fu&target_lang=RU"),
    assertRequest(ru, ["simple text"], #{ignore_tags => ["fu", "bar"]}, "text=simple%20text&ignore_tags=fu%2Cbar&target_lang=RU"),

    ?assertEqual(
        {error, {badoption, {invalid, option}}},
        gen_deeperl_procedure:request(deeperl_translate, "authkey", [en, ["text"], #{invalid => option}])
    ),

    ok.

response_test() ->
    [
        ?assertEqual(
            [{en,"Das ist der erste Satz."}, {en,"Das ist der zweite Satz."}, {en,"Dies ist der dritte Satz."}],
            gen_deeperl_procedure:response(deeperl_translate, <<"{\"translations\": [{\"detected_source_language\":\"EN\", \"text\":\"Das ist der erste Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Das ist der zweite Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Dies ist der dritte Satz.\"}]}">>)
        ),
        ?assertEqual(
            {error, {invalid_response, <<"{\"translation\": [{\"detected_source_language\":\"EN\", \"text\":\"Das ist der erste Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Das ist der zweite Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Dies ist der dritte Satz.\"}]}">>}},
            gen_deeperl_procedure:response(deeperl_translate, <<"{\"translation\": [{\"detected_source_language\":\"EN\", \"text\":\"Das ist der erste Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Das ist der zweite Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Dies ist der dritte Satz.\"}]}">>)
        ),
        ?assertEqual(
            {error, {invalid_response, <<"{\"translations\": [{\"detected_source_languag\":\"EN\", \"text\":\"Das ist der erste Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Das ist der zweite Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Dies ist der dritte Satz.\"}]}">>}},
            gen_deeperl_procedure:response(deeperl_translate, <<"{\"translations\": [{\"detected_source_languag\":\"EN\", \"text\":\"Das ist der erste Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Das ist der zweite Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Dies ist der dritte Satz.\"}]}">>)
        ),
        ?assertEqual(
            {error, {invalid_json, <<"{\"translations\": [{\"detected_source_language\"\"EN\", \"text\":\"Das ist der erste Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Das ist der zweite Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Dies ist der dritte Satz.\"}]}">>}},
            gen_deeperl_procedure:response(deeperl_translate, <<"{\"translations\": [{\"detected_source_language\"\"EN\", \"text\":\"Das ist der erste Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Das ist der zweite Satz.\"},{\"detected_source_language\":\"EN\", \"text\":\"Dies ist der dritte Satz.\"}]}">>)
        )
    ].

assertRequest(TargetLang, Text, Options, ExpectedBody) ->
    {Uri, Header, Body} = gen_deeperl_procedure:request(deeperl_translate, "authkey", [TargetLang,Text, Options]),

    ?assertEqual("/v2/translate", Uri),
    ?assertEqual(?DEFAULT_HEADER, Header),
    ?assertEqual("auth_key=authkey&" ++ ExpectedBody, Body).
