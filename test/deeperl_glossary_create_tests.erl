-module(deeperl_glossary_create_tests).
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    ?assertEqual(
        {
            post,
            {
                "/v2/glossaries",
                [],
                "application/x-www-form-urlencoded; charset=utf-8",
                "name=Name&source_lang=SourceLang&target_lang=TargetLang&entries=transl1%09t%C3%83%C2%A4xt1%0Atransl2%09text+2%0Atransl3%09text+%26+3%7D&entries_format=tsv"
            }
        },
        deeperl_glossary_create:request({
            <<"Name">>,
            "SourceLang",
            "TargetLang",
            [
                {<<"transl1">>, <<"täxt1"/utf8>>},
                {"transl2", <<"text 2">>},
                {<<"transl3"/utf8>>, "text & 3}"}
            ]
        })
    ).

response_test() ->
    ApiResponse = jiffy:encode(#{
        <<"glossary_id">> => <<"c27f8f1d-69f7-4ffe-b3f6-822c34128987">>,
        <<"name">> => <<"Name">>,
        <<"source_lang">> => <<"fu">>,
        <<"target_lang">> => <<"bar">>,
        <<"creation_time">> => <<"Mo 4. Okt 11:06:43 CEST 2021">>,
        <<"entry_count">> => 123
    }),

    ?assertEqual(
        {ok, #{
            id => "c27f8f1d-69f7-4ffe-b3f6-822c34128987",
            name => <<"Name">>,
            source_lang => "fu",
            target_lang => "bar",
            creation_time => "Mo 4. Okt 11:06:43 CEST 2021",
            entry_count => 123
        }},
        deeperl_glossary_create:response(ApiResponse)
    ).

