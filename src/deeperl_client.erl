%% @private
-module(deeperl_client).

-export([
    call/2
]).

-spec call ({atom() | pid(), nonempty_string()}, {module(), tuple()}) -> term().
call({HttpcProfile, AuthKey}, {DeeplModule, Arguments}) ->
    {Method, RequestData} = DeeplModule:request(Arguments),

    Request = case RequestData of
                  {Route, Headers, ContentType, Body} ->
                      {url(AuthKey, Route), Headers ++ headers(AuthKey), ContentType, Body};
                  {Route, Headers} -> {url(AuthKey, Route), Headers ++ headers(AuthKey)}
              end,

    {ok, Response} = httpc:request(Method, Request, [], [], HttpcProfile),

    {{_, StatusCode, StatusMessage}, _, ResponseBody} = Response,

    case StatusCode of
        400 -> {error, badrequest, StatusMessage};
        401 -> {error, authfailed, StatusMessage};
        403 -> {error, forbidden, StatusMessage};
        404 -> {error, notfound, StatusMessage};
        413 -> {error, requestsize, StatusMessage};
        415 -> {error, notsupported, StatusMessage};
        429 -> {error, toomanyrequests, StatusMessage};
        456 -> {error, quotaexceeded, StatusMessage};
        503 -> {error, resourceunavailable, StatusMessage};
        529 -> {error, toomanyrequests, StatusMessage};
        Code when Code >= 500 -> {error, internalerror, {StatusCode, StatusMessage}};
        Code when Code >= 200, Code =< 299 -> DeeplModule:response(ResponseBody)
    end.

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
        {"User-Agent", "deeperl/" ++ vsn() ++ " (https://codeberg.org/l-x/deeperl)"}
    ].

vsn() ->
    case application:get_key(vsn) of
        {ok, Vsn} -> Vsn;
        _ -> "unknown"
    end.
