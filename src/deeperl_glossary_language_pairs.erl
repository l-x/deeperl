%% @private
-module(deeperl_glossary_language_pairs).
-behaviour(gen_deeperl_method).

%% API
-export([request/1, response/1]).

request({}) ->
    {
        get,
        {
            "/v2/glossary-language-pairs",
            []
        }
    }.

response(Body) ->
    Result = jiffy:decode(Body, [return_maps]),
    {ok, [
        {
            binary_to_list(maps:get(<<"source_lang">>, Map)),
            binary_to_list(maps:get(<<"target_lang">>, Map))
        } || Map <- maps:get(<<"supported_languages">>, Result)
    ]}.

