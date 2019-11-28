%%% @private

-module(deeperl_usage).
-behavior(gen_deeperl_procedure).

%% API
-export([uri/0, body_params/1, response/1]).

uri() ->
    "/v2/usage".
body_params(_) ->
    [].

response(#{<<"character_count">> := CharacterCount, <<"character_limit">> := CharacterLimit}) when is_integer(CharacterCount), is_integer(CharacterLimit)->
    {CharacterCount, CharacterLimit};
response(_) ->
    {error, invalid_response}.
