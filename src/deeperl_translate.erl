%% @private
-module(deeperl_translate).
-behaviour(gen_deeperl_method).

%% API
-export([request/1, response/1]).

request({TargetLanguage, Texts, #{} = TranslationOptions}) ->
    TextParams = [{"text", Text} || Text <- Texts],
    TargetLangParam = [{"target_lang", TargetLanguage}],
    TranslationParams = [translation_option(Name, Value) || {Name, Value} <- maps:to_list(TranslationOptions)],

    {
        post,
        {
            "/v2/translate",
            [],
            "application/x-www-form-urlencoded; charset=utf-8",
            uri_string:compose_query(TextParams ++ TargetLangParam ++ TranslationParams)
        }
    }.

response(Body) ->
    Response = jiffy:decode(Body, [return_maps]),

    {ok, [{
        unicode:characters_to_list(maps:get(<<"detected_source_language">>, Map)),
        maps:get(<<"text">>, Map)
    } || Map <- maps:get(<<"translations">>, Response)]}.


%% Internal

translation_option(source_lang, SourceLang) ->
    {"source_lang", SourceLang};

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

translation_option(ignore_tags, TagList) ->
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
    {"glossary_id", GlossaryId}.
