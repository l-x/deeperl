%%% @private

-module(deeperl_translate).
-behavior(gen_deeperl_procedure).

%% API
-export([uri/0, body/1, response/1]).
-export_type([result/0]).

-type result() :: [{DetectedSourceLanguage :: deeperl:language(), Text :: binary()}] | {error, invalid_response} | {error, {bad_option, Option :: any()}}.

uri() ->
    "/v2/translate".

-spec body({TargetLang :: deeperl:language(), Texts :: [iodata()], Options :: deeperl:translation_options()}) -> iodata().
body({TargetLang, [T|_] = Texts, Options}) when is_list(T) ->
    case format_translate_options(maps:to_list(Options), []) of
        {error, _} = E -> E;
        O ->
            [
                [["&text=", http_uri:encode(Text)] || Text <- Texts],
                [["&", K, "=", V] || {K, V} <- O],
                "&target_lang=", http_uri:encode(string:uppercase(atom_to_list(TargetLang)))
            ]
    end;
body({_, _, _}) ->
    {error, bad_text_format}.

-spec response(Data :: map()) -> deeperl:translate_result().
response(#{<<"translations">> := Translations}) ->
    response(Translations, []);
response(_) ->
    {error, invalid_response}.

%% internal
-spec response(Data :: map(), Acc :: deeperl:translate_result()) -> deeperl:translate_result().
response([#{<<"detected_source_language">> := Language, <<"text">> := Text}|Rest], Acc) ->
    Item = {list_to_atom(string:lowercase(unicode:characters_to_list(Language))), iolist_to_binary(Text)},
    response(Rest, [Item|Acc]);
response([], Acc) ->
    lists:reverse(Acc);
response(_, _) ->
    {error, invalid_response}.

format_translate_options([Option|Options], Acc) ->
    case format_translate_option(Option) of
        {error, _Reason} = E -> E;
        O -> format_translate_options(Options, [O|Acc])
    end;
format_translate_options([], Acc) ->
    Acc.

format_translate_option({source_lang, Value}) ->
    {"source_lang", string:uppercase(atom_to_list(Value))};
format_translate_option({split_sentences, Value}) when is_boolean(Value); Value == nonewlines ->
    {"split_sentences", case Value of
        true -> "1";
        false -> "0";
        nonewlines -> "nonewlines"
    end};
format_translate_option({preserve_formatting, Value}) when is_boolean(Value) ->
    {"preserve_formatting", case Value of
        true -> "1";
        false -> "0"
    end};
format_translate_option({tag_handling, xml}) ->
    {"tag_handling", "xml"};
format_translate_option({outline_detection, Value}) when is_boolean(Value) ->
    {"outline_detection", case Value of
        true -> "1";
        false -> "0"
    end};
format_translate_option({non_splitting_tags, Value}) when is_list(Value) ->
    {"non_splitting_tags", string:join(lists:map(fun http_uri:encode/1, Value), ",")};
format_translate_option({splitting_tags, Value}) when is_list(Value) ->
    {"splitting_tags", string:join(lists:map(fun http_uri:encode/1, Value), ",")};
format_translate_option({ignore_tags, Value}) when is_list(Value) ->
    {"ignore_tags", string:join(lists:map(fun http_uri:encode/1, Value), ",")};
format_translate_option(Option) ->
    {error, {bad_option, Option}}.
