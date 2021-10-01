%% @private
-module(deeperl_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ChildSpec = #{
        id => deeperl,
        start => {deeperl, start_link, []},
        restart => permanent,
        type => worker
    },
    {ok, {
        {one_for_all, 1, 5},
        [ChildSpec]
    }}.
