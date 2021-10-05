%% @private
-module(deeperl_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    AuthKey = case application:get_env(auth_key) of
                  {ok, Value} -> Value;
                  undefined -> ""
              end,

    deeperl_sup:start_link(#{
        auth_key => AuthKey
    }).

stop(_State) ->
    ok.
