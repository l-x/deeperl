%%% @private

-module(deeperl_languages).
-behavior(gen_deeperl_procedure).

%% API
-export([uri/0, body_params/1, response/1]).

uri() ->
    "/v2/languages".
body_params(_) ->
    [].

response(L) ->
    response(L, []).

response([#{<<"language">> := Language, <<"name">> := Name}|R], Acc) ->
    Item = {list_to_atom(string:lowercase(unicode:characters_to_list(Language))), unicode:characters_to_list(Name)},
    response(R, [Item|Acc]);
response([], Acc) ->
    lists:reverse(Acc);
response(_, _Acc) ->
    {error, invalid_response}.
