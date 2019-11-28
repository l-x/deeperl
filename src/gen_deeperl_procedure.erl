%%% @private

-module(gen_deeperl_procedure).

%% Callbacks

-callback uri() -> Uri :: nonempty_string().
-callback body_params(Args :: list()) -> [{Name :: nonempty_string(), Value :: nonempty_string()}].
-callback response(Response :: term()) -> term().

%% API
-export([request/3, response/2]).

request(Module, AuthKey, Args) ->
    case body(Module, AuthKey, Args) of
        {error, _Reason} = E -> E;
        Body -> {uri(Module), header(Module), Body}
    end.

response(Module, Response) ->
    case catch Module:response(jiffy:decode(Response, [return_maps])) of
        {'EXIT', {{_, E}, _}} -> {error, {E, Response}};
        {error, E} -> {error, {E, Response}};
        R -> R
    end.

uri(Module) ->
    Module:uri().

header(_Module) ->
    [
        {<<"content-type">>, "application/x-www-form-urlencoded"},
        {<<"user-agent">>, "deeperl/0.1.0"}
    ].


body(Module, AuthKey, Args) ->
    ModParams = Module:body_params(Args),

    case Module:body_params(Args) of
        {error, _Reason} = E -> E;
        ModParams ->
            Params = [{"auth_key", AuthKey}|ModParams],
            ParamLines = [io_lib:format("~s=~s", [K, http_uri:encode(V)]) || {K, V} <- Params],
            lists:flatten(string:join(ParamLines, "&"))
    end.
