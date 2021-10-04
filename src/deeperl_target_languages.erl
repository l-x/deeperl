%% @private
-module(deeperl_target_languages).
-behaviour(gen_deeperl_method).

%% API
-export([request/1, response/1]).

request({}) ->
    {
        post,
        {
            "/v2/languages",
            [],
            "application/x-www-form-urlencoded; charset=utf-8",
            "type=target"
        }
    }.

response(Body) ->
    Response = jiffy:decode(Body, [return_maps]),

    {ok, [{
        unicode:characters_to_list(maps:get(<<"language">>, Map)),
        unicode:characters_to_list(maps:get(<<"name">>, Map)),
        maps:get(<<"supports_formality">>, Map)
    } || Map <- Response]}.
