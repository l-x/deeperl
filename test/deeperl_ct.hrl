-include_lib("common_test/include/ct.hrl").

-define(DEFAULT_HEADERS, [
    {"Authorization", "DeepL-Auth-Key "},
    {"User-Agent", "deeperl/0.9.0 (https://codeberg.org/l-x/deeperl)"}
]).

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(deeperl),

    ok = meck:new(httpc),

    Config.

end_per_suite(Config) ->
    ok = application:stop(deeperl),
    ok = meck:unload(httpc),
    Config.

expect_request(RequestMethod, Request, ResponseBody) ->
    MockFun = fun(ActualRequestMethod, ActualRequest, [], [], default) ->
        ActualRequestMethod = RequestMethod,
        ActualRequest = Request,

        {ok, {{ok, 200, "status message"}, {}, ResponseBody}}
     end,

    meck:expect(httpc, request, MockFun).
