%% @private
-module(deeperl_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    AuthKey = case application:get_env(auth_key) of
                  {ok, X} -> X;
                  undefined -> ""
              end,

    HttpcProfile = case application:get_env(httpc_profile) of
                  {ok, Y} -> Y;
                  undefined -> default
              end,

    deeperl_sup:start_link(#{
        auth_key => AuthKey,
        httpc_profile => HttpcProfile
    }).

stop(_State) ->
    ok.
