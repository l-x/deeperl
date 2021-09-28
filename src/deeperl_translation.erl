-module(deeperl_translation).

-export([
    source_languages/0,
    target_languages/0,
    translate/3,
    usage/0
]).

-define(
    FORM_ENCODING, 
    "application/x-www-form-urlencoded; charset=utf-8"
).

source_languages() ->
    {
        {
            post,
            {
                "/v2/languages",
                [],
                ?FORM_ENCODING,
                "type=source"
            }
        },
        fun(200, ResponseBody) ->
            ResponseBody
        end
    }.

target_languages() ->
    {
        {
            post,
            {
                "/v2/languages",
                [],
                ?FORM_ENCODING,
                "type=target"
            }
        },
        fun(200, ResponseBody) ->
            ResponseBody
        end
    }.

usage() ->
    {
        {
            post,
            {
                "/v2/usage",
                [],
                ?FORM_ENCODING,
                "type=source"
            }
        },
        fun(200, ResponseBody) ->
            ResponseBody
        end
    }.

translate(TargetLanguage, Texts, #{} = TranslationOptions) ->
    TextParams = [{"text", Text} || Text <- Texts],
    TargetLangParam = [{"target_lang", TargetLanguage}],
    TranslationParams = [translation_option(Name, Value) || {Name, Value} <- maps:to_list(TranslationOptions)],

    {
        {
            post,
            {
                "/v2/translate",
                [],
                ?FORM_ENCODING,
                uri_string:compose_query(TextParams ++ TargetLangParam ++ TranslationParams)
            }
        },
        fun(200, ResponseBody) ->
            ResponseBody
        end
    }.

translation_option(source_lang, SourceLang)-> {"source_lang", string:uppercase(SourceLang)};

translation_option(split_sentences, nonewlines) -> {"split_sentences", "nonewlines"};
translation_option(split_sentences, false) -> {"split_sentences", "0"};
translation_option(split_sentences, true) -> {"split_sentences", "1"};

translation_option(preserve_formatting, false) -> {"preserve_formatting", "0"};
translation_option(preserve_formatting, true) -> {"preserve_formatting", "1"};

translation_option(tag_handling, xml) -> {"tag_handling", "xml"};

translation_option(non_splitting_tags, TagList) -> {"non_splitting_tags", string:join(TagList, ",")};
translation_option(splitting_tags, TagList) -> {"splitting_tags", string:join(TagList, ",")};
translation_option(ingnore_tags, TagList) -> {"ignore_tags", string:join(TagList, ",")};

translation_option(outline_detection, false) -> {"outline_detection", "0"};
translation_option(outline_detection, true) -> {"outline_detection", "1"};

translation_option(formality, default) -> {"formality", "default"};
translation_option(formality, more) -> {"formality", "more"};
translation_option(formality, less) -> {"formality", "less"};

translation_option(glossary_id, GlossaryId) -> {"glossary_id", GlossaryId};

translation_option(Name, Value) -> throw(io_lib:format("Unknown or invalid translation option: '~s = ~s~n'", [Name, Value])).
