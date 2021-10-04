-module(deeperl_translate_tests).
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    ?assertEqual(
        {post, {
            "/v2/translate",
            [],
            "application/x-www-form-urlencoded; charset=utf-8",
            "text=t%C3%A4xt1&text=text+2&text=text+%26+3&target_lang=xy&formality=more&glossary_id=dd37e575-d0f8-4059-8ec8-693c1ec581c4&ignore_tags=tag5%2Ctag6&non_splitting_tags=tag1%2Ctag2&outline_detection=0&preserve_formatting=1&source_lang=yz&split_sentences=nonewlines&splitting_tags=tag3%2Ctag4&tag_handling=xml"}
        },

        deeperl_translate:request({
            "xy",
            [
                <<"täxt1"/utf8>>,
                <<"text 2">>,
                "text & 3"
            ],
            #{
                source_lang => "yz",
                split_sentences =>  nonewlines,
                preserve_formatting => true,
                tag_handling => xml,
                non_splitting_tags => ["tag1", "tag2"],
                splitting_tags => ["tag3", "tag4"],
                ignore_tags => ["tag5", "tag6"],
                outline_detection => false,
                formality => more,
                glossary_id => "dd37e575-d0f8-4059-8ec8-693c1ec581c4"
            }
        })
    ).

response_test() ->
    ApiResponse = jiffy:encode(
        #{translations => [
            #{detected_source_language => <<"lang1">>, text => <<"text1">>},
            #{detected_source_language => <<"lang2">>, text => <<"text2">>},
            #{detected_source_language => <<"lang3">>, text => <<"text3">>}
        ]}
    ),

    ?assertEqual(
        {ok, [
            {"lang1", <<"text1">>},
            {"lang2", <<"text2">>},
            {"lang3", <<"text3">>}
        ]},
        deeperl_translate:response(ApiResponse)
    ).
