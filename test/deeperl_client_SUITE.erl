-module(deeperl_client_SUITE).
-behavior(gen_deeperl_method).
-include_lib("deeperl_ct.hrl").

-compile(export_all).

all() ->
    [
        badrequest,
        authfailed,
        forbidden,
        notfound,
        requestsize,
        notsupported,
        toomanyrequests,
        quotaexceeded,
        resourceunavailable,
        internalerror
    ].

badrequest(_Config) ->
    test_for_error(400, ?FUNCTION_NAME).

authfailed(_Config) ->
    test_for_error(401, ?FUNCTION_NAME).

forbidden(_Config) ->
    test_for_error(403, ?FUNCTION_NAME).

notfound(_Config) ->
    test_for_error(404, ?FUNCTION_NAME).

requestsize(_Config) ->
    test_for_error(413, ?FUNCTION_NAME).

notsupported(_Config) ->
    test_for_error(415, ?FUNCTION_NAME).

toomanyrequests(_Config) ->
    % Not sure if this is no error in the official api docs
    test_for_error(429, ?FUNCTION_NAME),
    test_for_error(529, ?FUNCTION_NAME).

quotaexceeded(_Config) ->
    test_for_error(456, ?FUNCTION_NAME).

resourceunavailable(_Config) ->
    test_for_error(503, ?FUNCTION_NAME).

internalerror(_Config) ->
    MockFun = fun(_, _, _, _, _) ->
        {ok, {{ok, 500, "StatusMessage"}, {}, ""}}
              end,

    meck:expect(httpc, request, MockFun),
    {error, internalerror, {500, "StatusMessage"}} = deeperl_client:call({default, "auth_key"}, {?MODULE, {}}).

test_for_error(StatusCode, ExpectedReason) ->
    MockFun = fun(_, _, _, _, _) ->
        {ok, {{ok, StatusCode, "StatusMessage"}, {}, ""}}
    end,

    meck:expect(httpc, request, MockFun),
    {error, ExpectedReason, "StatusMessage"} = deeperl_client:call({default, "auth_key"}, {?MODULE, {}}).


request({}) ->
    {get, {"", []}}.

response(_Body) ->
    erlang:error(not_implemented).
