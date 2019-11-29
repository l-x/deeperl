%%% @private

-module(deeperl_languages).
-behavior(gen_deeperl_procedure).

%% API
-export([uri/0, body/1, response/1]).

-spec uri() -> nonempty_string().
uri() ->
    "/v2/languages".

-spec body(Args :: tuple()) -> iolist().
body(_) ->
    [].

-spec response(Data :: map()) -> deeperl:languages_result().
response(L) ->
    response(L, []).

-spec response(Data :: map(), Acc :: deeperl:languages_result()) -> deeperl:languages_result().
response([#{<<"language">> := Language, <<"name">> := Name}|R], Acc) ->
    Item = {list_to_atom(string:lowercase(unicode:characters_to_list(Language))), iolist_to_binary(Name)},
    response(R, [Item|Acc]);
response([], Acc) ->
    lists:reverse(Acc);
response(_, _Acc) ->
    {error, invalid_response}.
