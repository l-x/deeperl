%% @private
-module(deeperl_usage).
-behaviour(gen_deeperl_method).

%% API
-export([request/1, response/1]).

request({}) ->
    {
        post,
        {
            "/v2/usage",
            [],
            "application/x-www-form-urlencoded; charset=utf-8",
            ""
        }
    }.

response(Body) ->
    Response = jiffy:decode(Body, [return_maps]),

    {ok, {
        maps:get(<<"character_count">>, Response),
        maps:get(<<"character_limit">>, Response)
    }}.
