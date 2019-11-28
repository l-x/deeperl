%%% @private

-module(deeperl_translate).
-behavior(gen_deeperl_procedure).

%% API
-export([uri/0, body_params/1, response/1]).

uri() ->
    "/v2/translate".

body_params([TargetLang, [T|_] = Text, Options]) when is_list(T) ->
    case format_translate_options(maps:to_list(Options), []) of
        {error, _} = E -> E;
        O ->
            lists:merge([
                [{"target_lang", string:uppercase(atom_to_list(TargetLang))}],
                [{text, V} || V <- Text],
                O
            ])
    end.

response(#{<<"translations">> := Translations}) ->
    response(Translations, []);
response(_) ->
    {error, invalid_response}.

%% internal

response([#{<<"detected_source_language">> := Language, <<"text">> := Text}|Rest], Acc) ->
    Item = {list_to_atom(string:lowercase(unicode:characters_to_list(Language))), unicode:characters_to_list(Text)},
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
    {error, {badoption, Option}}.
