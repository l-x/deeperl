-module(deeperl_glossary_information_tests).
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    ?assertEqual(
        {
            get,
            {
                "/v2/glossaries/c27f8f1d-69f7-4ffe-b3f6-822c34128987",
                []
            }
        },
        deeperl_glossary_information:request({"c27f8f1d-69f7-4ffe-b3f6-822c34128987"})
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
        deeperl_glossary_information:response(ApiResponse)
    ).

