-module(deeperl_translation).

-export([
    source_languages/0,
    target_languages/0,
    translate/3,
    usage/0
]).

-define(FORM_ENCODING, "application/x-www-form-urlencoded; charset=utf-8").

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
        fun({200, _}, ResponseBody) ->
            Response = jiffy:decode(ResponseBody, [return_maps]),
            {ok, [{
                unicode:characters_to_list(maps:get(<<"language">>, Map)),
                unicode:characters_to_list(maps:get(<<"name">>, Map))
            } || Map <- Response]}
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
        fun({200, _}, ResponseBody) ->
            Response = jiffy:decode(ResponseBody, [return_maps]),
            {ok, [{
                unicode:characters_to_list(maps:get(<<"language">>, Map)),
                unicode:characters_to_list(maps:get(<<"name">>, Map)),
                maps:get(<<"supports_formality">>, Map)
            } || Map <- Response]}
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
        fun({200, _}, ResponseBody) ->
            Response = jiffy:decode(ResponseBody, [return_maps]),
            {ok, {
                maps:get(<<"character_count">>, Response),
                maps:get(<<"character_limit">>, Response)
            }}
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
        fun({200, _}, ResponseBody) ->
            Response = jiffy:decode(ResponseBody, [return_maps]),

            [{
                unicode:characters_to_list(maps:get(<<"detected_source_language">>, Map)),
                maps:get(<<"text">>, Map)
            } || Map <- maps:get(<<"translations">>, Response)]
        end
    }.

translation_option(source_lang, SourceLang)-> 
    {"source_lang", string:uppercase(SourceLang)};

translation_option(split_sentences, SplitSentences) ->
    {"split_sentences",
        case SplitSentences of
            nonewlines -> "nonewlines";
            true -> "1";
            false -> "0"
        end    
    };

translation_option(preserve_formatting, PreserveFormatting) ->
    {"preserve_formatting",
        case PreserveFormatting of
            true -> "1";
            false -> "0"
        end    
    };

translation_option(tag_handling, xml) -> 
    {"tag_handling", "xml"};

translation_option(non_splitting_tags, TagList) -> 
    {"non_splitting_tags", string:join(TagList, ",")};

translation_option(splitting_tags, TagList) -> 
    {"splitting_tags", string:join(TagList, ",")};

translation_option(ingnore_tags, TagList) -> 
    {"ignore_tags", string:join(TagList, ",")};

translation_option(outline_detection, OutlineDetection) -> 
    {"outline_detection",
        case OutlineDetection of
            true -> "1";
            false -> "0"
        end    
    };

translation_option(formality, Formality) ->
    {"formality", 
        case Formality of
            default -> "default";
            more -> "more";
            less -> "less"
        end
    };

translation_option(glossary_id, GlossaryId) -> 
    {"glossary_id", GlossaryId};

translation_option(Name, Value) -> 
    throw(io_lib:format("Unknown or invalid translation option: '~s = ~s~n'", [Name, Value])).
