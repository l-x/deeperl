%% @private
-module(deeperl_sup).
-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

-define(SERVER, ?MODULE).

start_link(#{} = Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Config]).

init([Config]) ->
    ChildSpec = #{
        id => deeperl,
        start => {deeperl, start_link, [Config]},
        restart => permanent,
        type => worker
    },
    {ok, {
        {one_for_all, 1, 5},
        [ChildSpec]
    }}.
