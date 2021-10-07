-module(deeperl_glossary_SUITE).
-include_lib("deeperl_ct.hrl").

-compile(export_all).

all() ->
    [
        glossary_list,
        glossary_information,
        glossary_create,
        glossary_entries,
        glossary_delete
    ].

glossary_list(_Config) ->
    Request = {
        "https://api.deepl.com/v2/glossaries",
        ?DEFAULT_HEADERS
    },

    ApiResponse = jiffy:encode(#{
        <<"glossaries">> => [#{
            <<"glossary_id">> => <<"c27f8f1d-69f7-4ffe-b3f6-822c34128987">>,
            <<"name">> => <<"Name">>,
            <<"source_lang">> => <<"fu">>,
            <<"target_lang">> => <<"bar">>,
            <<"creation_time">> => <<"Mo 4. Okt 11:06:43 CEST 2021">>,
            <<"entry_count">> => 123
        }]
    }),

    ExpectedResult = {ok, [#{
        id => "c27f8f1d-69f7-4ffe-b3f6-822c34128987",
        name => <<"Name">>,
        source_lang => "fu",
        target_lang => "bar",
        creation_time => "Mo 4. Okt 11:06:43 CEST 2021",
        entry_count => 123
    }]},

    expect_request(get, Request, ApiResponse),
    ExpectedResult = deeperl:glossary_list().

glossary_information(_Config) ->
    Request = {
        "https://api.deepl.com/v2/glossaries/c27f8f1d-69f7-4ffe-b3f6-822c34128987",
        ?DEFAULT_HEADERS
    },

    ApiResponse = jiffy:encode(#{
            <<"glossary_id">> => <<"c27f8f1d-69f7-4ffe-b3f6-822c34128987">>,
            <<"name">> => <<"Name">>,
            <<"source_lang">> => <<"fu">>,
            <<"target_lang">> => <<"bar">>,
            <<"creation_time">> => <<"Mo 4. Okt 11:06:43 CEST 2021">>,
            <<"entry_count">> => 123
        }
    ),

    ExpectedResult = {ok, #{
        id => "c27f8f1d-69f7-4ffe-b3f6-822c34128987",
        name => <<"Name">>,
        source_lang => "fu",
        target_lang => "bar",
        creation_time => "Mo 4. Okt 11:06:43 CEST 2021",
        entry_count => 123
    }},

    expect_request(get, Request, ApiResponse),
    ExpectedResult = deeperl:glossary_information("c27f8f1d-69f7-4ffe-b3f6-822c34128987").

glossary_create(_Config) ->
    Request = {
        "https://api.deepl.com/v2/glossaries",
        ?DEFAULT_HEADERS,
        "application/x-www-form-urlencoded; charset=utf-8",
        "name=Name&source_lang=SourceLang&target_lang=TargetLang&entries=transl1%09t%C3%83%C2%A4xt1%0Atransl2%09text+2%0Atransl3%09text+%26+3%7D&entries_format=tsv"
    },

    ApiResponse = jiffy:encode(#{
        <<"glossary_id">> => <<"c27f8f1d-69f7-4ffe-b3f6-822c34128987">>,
        <<"name">> => <<"Name">>,
        <<"source_lang">> => <<"SourceLang">>,
        <<"target_lang">> => <<"TargetLang">>,
        <<"creation_time">> => <<"Mo 4. Okt 11:06:43 CEST 2021">>,
        <<"entry_count">> => 123
    }),

    ExpectedResult = {ok, #{
        id => "c27f8f1d-69f7-4ffe-b3f6-822c34128987",
        name => <<"Name">>,
        source_lang => "SourceLang",
        target_lang => "TargetLang",
        creation_time => "Mo 4. Okt 11:06:43 CEST 2021",
        entry_count => 123
    }},

    expect_request(post, Request, ApiResponse),

    ExpectedResult = deeperl:glossary_create(
        <<"Name">>,
        "SourceLang",
        "TargetLang",
        [
            {<<"transl1">>, <<"täxt1"/utf8>>},
            {"transl2", <<"text 2">>},
            {<<"transl3"/utf8>>, "text & 3}"}
        ]
    ).

glossary_entries(_Config) ->
    Request = {
        "https://api.deepl.com/v2/glossaries/c27f8f1d-69f7-4ffe-b3f6-822c34128987/entries",
        ?DEFAULT_HEADERS
    },

    ApiResponse = <<"fu\tbar\nherp\tderp"/utf8>>,

    expect_request(get, Request, ApiResponse),

    {ok, [
        {<<"fu">>, <<"bar">>},
        {<<"herp">>, <<"derp">>}
    ]} = deeperl:glossary_entries("c27f8f1d-69f7-4ffe-b3f6-822c34128987").

glossary_delete(_Config) ->
    Request = {
        "https://api.deepl.com/v2/glossaries/c27f8f1d-69f7-4ffe-b3f6-822c34128987",
        ?DEFAULT_HEADERS
    },

    expect_request(delete, Request, <<"we don't care">>),

    ok = deeperl:glossary_delete("c27f8f1d-69f7-4ffe-b3f6-822c34128987").
