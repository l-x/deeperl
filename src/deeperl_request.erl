-module(deeperl_request).

%% API
-export([usage/1, languages/1, translate/4]).

-type request_data() :: {Url :: nonempty_string(), Headers :: [{Name :: iodata(), Value :: string()}], Body :: nonempty_string()}.

-spec usage(AuthKey :: nonempty_string()) -> request_data().
usage(AuthKey) ->
    {"/v2/usage", headers(), body(AuthKey)}.

-spec languages(AuthKey :: nonempty_string()) -> request_data().
languages(AuthKey) ->
    {"/v2/languages", headers(), body(AuthKey)}.

-spec translate(
    AuthKey :: nonempty_string(),
    TargetLang :: deeperl:language(),
    Text :: nonempty_string() | [nonempty_string()],
    Options :: deeperl:translation_options()
) -> request_data().
translate(AuthKey, TargetLang, [T|_] = Text, Options) when is_list(T) == false->
    translate(AuthKey, TargetLang, [Text], Options);
translate(AuthKey, TargetLang, Text, Options) ->
    {"/v2/translate", headers(), body(AuthKey, TargetLang, Text, Options)}.

%% Internal

headers() ->
    [
        {<<"content-type">>, "application/x-www-form-urlencoded"},
        {<<"user-agent">>, "deeperl/0.1.0"}
    ].

body(AuthKey) ->
    "auth_key=" ++ AuthKey.

body(AuthKey, TargetLang, Text, Options) ->
    MandatoryOptions = [
            body(AuthKey),
            "target_lang=" ++ http_uri:encode(string:uppercase(atom_to_list(TargetLang)))
    ],

    Texts = lists:map(
        fun(T) -> "text=" ++ http_uri:encode(T) end,
        Text
    ),

    OptionPairs = lists:map(
        fun (Option) -> format_translate_option(Option) end,
        maps:to_list(Options)
    ),

    string:join(lists:merge([OptionPairs, Texts, MandatoryOptions]), "&").

format_translate_option({source_lang, Value}) ->
    "source_lang=" ++ http_uri:encode(string:uppercase(atom_to_list(Value)));
format_translate_option({split_sentences, Value}) ->
    "split_sentences=" ++ case Value of
        true -> "1";
        false -> "0";
        nonewlines -> "nonewlines"
    end;
format_translate_option({preserve_formatting, Value}) ->
    "preserve_formatting=" ++ case Value of
        true -> "1";
        false -> "0"
    end;
format_translate_option({tag_handling, xml}) ->
    "tag_handling=xml";
format_translate_option({outline_detection, Value}) ->
    "outline_detection=" ++ case Value of
        true -> "1";
        false -> "0"
    end;
format_translate_option({non_splitting_tags, Value}) ->
    "non_splitting_tags=" ++ string:join(lists:map(fun (E) -> http_uri:encode(E) end, Value), ",");
format_translate_option({splitting_tags, Value}) ->
    "splitting_tags=" ++ string:join(lists:map(fun (E) -> http_uri:encode(E) end, Value), ",");
format_translate_option({ignore_tags, Value}) ->
    "ignore_tags=" ++ string:join(lists:map(fun (E) -> http_uri:encode(E) end, Value), ",");
format_translate_option(_) ->
    erlang:error(badoption).
