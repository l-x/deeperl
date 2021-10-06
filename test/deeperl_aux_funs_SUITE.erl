-module(deeperl_aux_funs_SUITE).
-include_lib("deeperl_ct.hrl").

-compile(export_all).

all() ->
    [
        usage,
        source_languages,
        target_languages
    ].

usage(_Config) ->
    Request = {
        "https://api.deepl.com/v2/usage",
        ?DEFAULT_HEADERS,
        "application/x-www-form-urlencoded; charset=utf-8",
        ""
    },

    expect_request(post, Request, <<"{\"character_count\": 123,\"character_limit\": 456}">>),

    {ok, {123, 456}} = deeperl:usage().

source_languages(_Config) ->
    Request = {
        "https://api.deepl.com/v2/languages",
        ?DEFAULT_HEADERS,
        "application/x-www-form-urlencoded; charset=utf-8",
        "type=source"
    },

    ApiResponse = jiffy:encode([
        #{language => <<"lang1">>, name => <<"name1">>},
        #{language => <<"lang2">>, name => <<"name2">>},
        #{language => <<"lang3">>, name => <<"name3">>}
    ]),

    expect_request(post, Request, ApiResponse),

    {ok, [
        {"lang1", "name1"},
        {"lang2", "name2"},
        {"lang3", "name3"}
    ]} = deeperl:source_languages().

target_languages(_Config) ->
    Request = {
        "https://api.deepl.com/v2/languages",
        ?DEFAULT_HEADERS,
        "application/x-www-form-urlencoded; charset=utf-8",
        "type=target"
    },

    ApiResponse = jiffy:encode([
        #{language => <<"lang1">>, name => <<"name1">>, supports_formality => true},
        #{language => <<"lang2">>, name => <<"name2">>, supports_formality => false},
        #{language => <<"lang3">>, name => <<"name3">>, supports_formality => false}
    ]),

    expect_request(post, Request, ApiResponse),

    {ok, [
        {"lang1", "name1", true},
        {"lang2", "name2", false},
        {"lang3", "name3", false}
    ]} = deeperl:target_languages().
