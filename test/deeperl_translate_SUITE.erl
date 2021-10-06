-module(deeperl_translate_SUITE).
-include_lib("deeperl_ct.hrl").

-compile(export_all).

all() ->
    [
        simple_translate,
        translate_source_lang,
        translate_split_sentences,
        translate_preserve_formatting,
        translate_tag_handling,
        translate_non_splitting_tags,
        translate_splitting_tags,
        translate_ignore_tags,
        translate_outline_detection,
        translate_formality,
        translate_glossary_id
    ].

expect_translate(RequestBody) ->
    Request = {
        "https://api.deepl.com/v2/translate",
        ?DEFAULT_HEADERS,
        "application/x-www-form-urlencoded; charset=utf-8",
        RequestBody
    },

    ApiResponse = jiffy:encode(#{
        <<"translations">> => [#{
            <<"detected_source_language">> => <<"SourceLang">>,
            <<"text">> => <<"translation">>
        }]
    }),

    expect_request(post, Request, ApiResponse).

test_translate_with_options(#{} = Options, RequestBodyOptions) ->
    expect_translate("text=some&text=text&target_lang=xy&" ++ RequestBodyOptions),
    {ok, [{"SourceLang", <<"translation">>}]} = deeperl:translate("xy", ["some", "text"], Options).

simple_translate(_Config) ->
    expect_translate("text=some&text=text&target_lang=xy"),
    {ok, [{"SourceLang", <<"translation">>}]} = deeperl:translate("xy", ["some", "text"]).

translate_source_lang(_Config) ->
    test_translate_with_options(#{source_lang => "yz"}, "source_lang=yz").

translate_split_sentences(_Config)->
    test_translate_with_options(#{split_sentences => false}, "split_sentences=0"),
    test_translate_with_options(#{split_sentences => true}, "split_sentences=1"),
    test_translate_with_options(#{split_sentences => nonewlines}, "split_sentences=nonewlines").

translate_preserve_formatting(_Config)->
    test_translate_with_options(#{preserve_formatting => false}, "preserve_formatting=0"),
    test_translate_with_options(#{preserve_formatting => true}, "preserve_formatting=1").

translate_tag_handling(_Config)->
    test_translate_with_options(#{tag_handling => xml}, "tag_handling=xml").

translate_non_splitting_tags(_Config)->
    test_translate_with_options(#{non_splitting_tags => ["fu", "bar"]}, "non_splitting_tags=fu%2Cbar").

translate_splitting_tags(_Config)->
    test_translate_with_options(#{splitting_tags => ["fu", "bar"]}, "splitting_tags=fu%2Cbar").

translate_ignore_tags(_Config)->
    test_translate_with_options(#{ignore_tags => ["fu", "bar"]}, "ignore_tags=fu%2Cbar").

translate_outline_detection(_Config)->
    test_translate_with_options(#{outline_detection => false}, "outline_detection=0"),
    test_translate_with_options(#{outline_detection => true}, "outline_detection=1").

translate_formality(_Config)->
    test_translate_with_options(#{formality => default}, "formality=default"),
    test_translate_with_options(#{formality => more}, "formality=more"),
    test_translate_with_options(#{formality => less}, "formality=less").

translate_glossary_id(_Config) ->
    test_translate_with_options(#{glossary_id => "GlossaryId"}, "glossary_id=GlossaryId").
