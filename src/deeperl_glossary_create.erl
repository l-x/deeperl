%% @private
-module(deeperl_glossary_create).
-behaviour(gen_deeperl_method).

%% API
-export([request/1, response/1]).

request({Name, SourceLang, TargetLang, Entries}) ->
    Params = [
        {"name", Name},
        {"source_lang", SourceLang},
        {"target_lang", TargetLang},
        {"entries", string:join([binary_to_list(iolist_to_binary([From, "\t", To])) || {From, To} <- Entries], "\n")},
        {"entries_format", "tsv"}
    ],

    {
        post,
        {
            "/v2/glossaries",
            [],
            "application/x-www-form-urlencoded; charset=utf-8",
            uri_string:compose_query(Params)
        }
    }.

response(Body) ->
    Result = jiffy:decode(Body, [return_maps]),

    {ok,
        deeperl_glossary:create(Result)
    }.
