-module(deeperl_request_tests).

-include_lib("eunit/include/eunit.hrl").
-define(DEFAULT_HEADERS, [
    {<<"content-type">>, "application/x-www-form-urlencoded"},
    {<<"user-agent">>, "deeperl/0.1.0"}
]).

usage_test() ->
    ?assertEqual(deeperl_request:usage("authkey"), {"/v2/usage", ?DEFAULT_HEADERS, "auth_key=authkey"}).
languages_test() ->
    ?assertEqual(deeperl_request:languages("authkey"), {"/v2/languages", ?DEFAULT_HEADERS, "auth_key=authkey"}).
translate_test() ->
    [
        ?assertEqual(
            deeperl_request:translate("authkey", en, "some text", #{}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&target_lang=EN&text=some%20text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, ["some text"], #{}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&target_lang=EN&text=some%20text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, ["some text", "another text"], #{}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&target_lang=EN&text=some%20text&text=another%20text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, ["some text", "another text"], #{source_lang => it, tag_handling => xml, non_splitting_tags => ["fu", "bar"], splitting_tags => ["splitting"], ignore_tags => []}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&ignore_tags=&non_splitting_tags=fu,bar&source_lang=IT&splitting_tags=splitting&tag_handling=xml&target_lang=EN&text=some%20text&text=another%20text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, "text", #{split_sentences => true}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&split_sentences=1&target_lang=EN&text=text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, "text", #{split_sentences => false}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&split_sentences=0&target_lang=EN&text=text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, "text", #{split_sentences => nonewlines}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&split_sentences=nonewlines&target_lang=EN&text=text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, "text", #{preserve_formatting => true}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&preserve_formatting=1&target_lang=EN&text=text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, "text", #{preserve_formatting => false}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&preserve_formatting=0&target_lang=EN&text=text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, "text", #{outline_detection => true}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&outline_detection=1&target_lang=EN&text=text"}
        ),
        ?assertEqual(
            deeperl_request:translate("authkey", en, "text", #{outline_detection => false}),
            {"/v2/translate", ?DEFAULT_HEADERS, "auth_key=authkey&outline_detection=0&target_lang=EN&text=text"}
        ),
        ?assertError(
            badoption,
            deeperl_request:translate("authkey", en, "text", #{unknown => option})
        )
    ].
