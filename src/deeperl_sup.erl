%%% @private

-module(deeperl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    ChildSpec = #{
        id => deeperl,
        start => {deeperl, start_link, []},
        restart => permanent,
        type => worker
    },
    {ok, { {one_for_all, 1, 5}, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
