-module(deeperl_glossary_entries_tests).
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    ?assertEqual(
        {
            get,
            {
                "/v2/glossaries/c27f8f1d-69f7-4ffe-b3f6-822c34128987/entries",
                []
            }
        },
        deeperl_glossary_entries:request({"c27f8f1d-69f7-4ffe-b3f6-822c34128987"})
    ).

response_test() ->
    ApiResponse = <<"fu\tbar\nherp\tderp"/utf8>>,

    ?assertEqual(
        {ok, [
            {<<"fu">>, <<"bar">>},
            {<<"herp">>, <<"derp">>}
        ]},
        deeperl_glossary_entries:response(ApiResponse)
    ).

