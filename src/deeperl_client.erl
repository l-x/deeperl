-module(deeperl_client).

-export([call/2]).

host(AuthKey) -> 
    case string:right(AuthKey, 3) of
        ":fx" -> "api-free.deepl.com";
        _ -> "api.deepl.com"
    end.

url(AuthKey, Route) ->
    "https://" ++ host(AuthKey) ++ Route.

headers(AuthKey) ->
    [
        {"Authorization", "DeepL-Auth-Key " ++ AuthKey},
        {"User-Agent", "deeperl/dev"}
    ].

call(AuthKey, {{Method, {Route, Headers, ContentType, Body}}, ResultFun}) ->
    {ok, Response} = httpc:request(
        Method,
        {
            url(AuthKey, Route),
            Headers ++ headers(AuthKey),
            ContentType,
            Body
        },
        [],
        []
    ),

    {{_, StatusCode, _}, _, ResponseBody} = Response,

    ResultFun(StatusCode, ResponseBody);

call(AuthKey, {{Method, {Route, Headers}}, ResultFun}) ->
    {ok, Response} = httpc:request(
        Method,
        {
            url(AuthKey, Route),
            Headers ++ headers(AuthKey)
        },
        [],
        []
    ),

    {{_, StatusCode, _}, _, ResponseBody} = Response,
    ResultFun(StatusCode, ResponseBody).