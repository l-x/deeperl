%% @private
-module(deeperl_client).

-export([
    call/2
]).

call(AuthKey, {{Method, RequestData}, ResultFun}) ->
    Request = case RequestData of
                  {Route, Headers, ContentType, Body} ->
                      {url(AuthKey, Route), Headers ++ headers(AuthKey), ContentType, Body};
                  {Route, Headers} -> {url(AuthKey, Route), Headers ++ headers(AuthKey)};
                  {Route} -> {url(AuthKey, Route), headers(AuthKey)}
              end,


    {ok, Response} = httpc:request(Method, Request, [], []),

    {{_, StatusCode, StatusMessage}, _, ResponseBody} = Response,

    ResultFun({StatusCode, StatusMessage}, ResponseBody).

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
