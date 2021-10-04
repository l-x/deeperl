%% @private
-module(deeperl_glossary_delete).
-behaviour(gen_deeperl_method).

%% API
-export([request/1, response/1]).

request({GlossaryId}) ->
    {
        delete,
        {
            "/v2/glossaries/" ++ GlossaryId,
            []
        }
    }.

response(_Body) ->
    ok.
