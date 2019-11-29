%%% @private

-module(gen_deeperl_procedure).

%% Callbacks

-callback uri() -> Uri :: nonempty_string().
-callback body(Args :: tuple()) -> iodata().
-callback response(Response :: map()) -> term().

%% API
-export([request/3, response/2]).

-spec request(Module :: module(), AuthKey :: nonempty_string(), Args :: term()) ->
    {Uri :: nonempty_string(), Header :: [{Name :: binary(), Value :: iodata()}], Body :: iodata()} | {error, term()}.
request(Module, AuthKey, Args) ->
    case body(Module, AuthKey, Args) of
        {error, _Reason} = E -> E;
        Body -> {uri(Module), header(Module), Body}
    end.

-spec response(Module :: module(), Response :: binary()) -> term().
response(Module, Response) ->
    case catch Module:response(jiffy:decode(Response, [return_maps])) of
        {'EXIT', {{_, E}, _}} -> {error, {E, Response}};
        {error, E} -> {error, {E, Response}};
        R -> R
    end.

-spec uri(Module :: module()) -> nonempty_string().
uri(Module) ->
    Module:uri().

-spec header(Module :: module()) -> [{Name :: binary(), Value :: iodata()}].
header(_Module) ->
    [
        {<<"content-type">>, "application/x-www-form-urlencoded; charset=utf-8"},
        {<<"user-agent">>, "deeperl/0.1.0"}
    ].

-spec body(Module :: module(), AuthKey :: nonempty_string(), Args :: term()) -> iodata() | {error, term()}.
body(Module, AuthKey, Args) ->
    case Module:body(Args) of
        {error, _Reason} = E -> E;
        ModParams -> [["auth_key=", AuthKey]|ModParams]
    end.
