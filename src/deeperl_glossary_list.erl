%% @private
-module(deeperl_glossary_list).
-behaviour(gen_deeperl_method).

%% API
-export([request/1, response/1]).

request({}) ->
    {
        get,
        {
            "/v2/glossaries",
            []
        }
    }.

response(Body) ->
    Result = jiffy:decode(Body, [return_maps]),

    {ok, [
        deeperl_glossary:create(Map) || Map <- maps:get(<<"glossaries">>, Result)
    ]}.
