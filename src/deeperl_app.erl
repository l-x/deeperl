%% @private
-module(deeperl_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    deeperl_sup:start_link(#{
        auth_key => application:get_env(deeperl, auth_key, ""),
        httpc_profile => application:get_env(deeperl, httpc_profile, default)
    }).

stop(_State) ->
    ok.
