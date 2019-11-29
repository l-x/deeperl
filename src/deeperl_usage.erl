%%% @private

-module(deeperl_usage).
-behavior(gen_deeperl_procedure).

%% API
-export([uri/0, body/1, response/1]).

-spec uri() -> nonempty_string().
uri() ->
    "/v2/usage".

-spec body(Args :: tuple()) -> iodata().
body(_) ->
    [].

-spec response(Data :: map()) -> deeperl:usage_result().
response(#{<<"character_count">> := CharacterCount, <<"character_limit">> := CharacterLimit}) when is_integer(CharacterCount), is_integer(CharacterLimit)->
    {CharacterCount, CharacterLimit};
response(_) ->
    {error, invalid_response}.
