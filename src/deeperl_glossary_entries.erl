%% @private
-module(deeperl_glossary_entries).
-behaviour(gen_deeperl_method).

%% API
-export([request/1, response/1]).

request({GlossaryId}) ->
    {
        get,
        {
            "/v2/glossaries/" ++ GlossaryId ++ "/entries",
            []
        }
    }.

response(Body) ->
    Lines = string:split(Body, "\n", all),

    {ok, [
        list_to_tuple(string:split(Line, "\t", all)) || Line <- Lines
    ]}.
