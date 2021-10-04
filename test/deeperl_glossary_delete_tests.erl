-module(deeperl_glossary_delete_tests).
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    ?assertEqual(
        {
            delete,
            {
                "/v2/glossaries/c27f8f1d-69f7-4ffe-b3f6-822c34128987",
                []
            }
        },
        deeperl_glossary_delete:request({"c27f8f1d-69f7-4ffe-b3f6-822c34128987"})
    ).

response_test() ->
    ?assertEqual(
        ok,
        deeperl_glossary_delete:response(<<"We don't care">>)
    ).

