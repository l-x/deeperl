%% @private
-module(deeperl_glossary_information).
-behaviour(gen_deeperl_method).

%% API
-export([request/1, response/1]).

request({GlossaryId}) ->
    {
        get,
        {
            "/v2/glossaries/" ++ GlossaryId,
            []
        }
    }.

response(Body) ->
    Result = jiffy:decode(Body, [return_maps]),

    {ok,
        deeperl_glossary:create(Result)
    }.
