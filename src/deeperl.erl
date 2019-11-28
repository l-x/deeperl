-module(deeperl).
-behaviour(gen_server).

%% API
-export([start_link/0, usage/0, languages/0, translate/2, translate/3]).
-export_type([translation_options/0]).

-type language() :: de | en | es | fr | it | nl | pl | pt | ru.
-type tag_list() :: [nonempty_string()].

-type translation_options() :: #{
    source_lang => language(),
    split_sentences => boolean() | nonewlines,
    preserve_formatting => boolean(),
    tag_handling => xml,
    non_splitting_tags =>  tag_list(),
    outline_detection => boolean(),
    splitting_tags => tag_list(),
    ignore_tags => tag_list()
}.

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    conn :: pid(),
    conn_mref :: erlang:monitor_process_identifier()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    GunOpts = case application:get_env(gun_opts) of
                  undefined -> #{};
                  {ok, Opts} -> Opts
              end,
    {ok, ConnPid} = gun:open("api.deepl.com", 443, GunOpts),
    {ok, #state{
        conn = ConnPid,
        conn_mref = monitor(process, ConnPid)
    }}.

handle_call(usage, _From, State) ->
    {reply, call(deeperl_usage, [], State), State};
handle_call(languages, _From, State) ->
    {reply, call(deeperl_languages, [], State), State};
handle_call({translate, TargetLang, Texts, Options}, _From, State) ->
    {reply, call(deeperl_translate, [TargetLang, Texts, Options], State), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', Mref, process, _ConnPid, Reason}, #state{conn_mref = Mref}) ->
    exit(Reason);
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

usage() ->
    gen_server:call(?SERVER, usage).

languages() ->
    gen_server:call(?SERVER, languages).

translate(TargetLang, Texts) ->
    translate(TargetLang, Texts, #{}).

translate(TargetLang, [T | _] = Texts, Options) when is_list(T) == false ->
    translate(TargetLang, [Texts], Options);
translate(TargetLang, Texts, Options) ->
    gen_server:call(?SERVER, {translate, TargetLang, Texts, Options}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

call(Module, Args, State) ->
    Request = case application:get_env(authkey) of
                  undefined -> {error, no_authkey};
                  {ok, AuthKey} -> gen_deeperl_procedure:request(Module, AuthKey, Args)
              end,
    Result = case Request of
                 {error, _Reason} = RequestError -> RequestError;
                 {Uri, Header, Body} -> request(Uri, Header, Body, State)
             end,

    case Result of
        {error, Reason} -> {error, Reason};
        {ok, R} -> gen_deeperl_procedure:response(Module, R)
    end.

request(Uri, Header, Body, State) ->
    StreamRef = gun:post(State#state.conn, Uri, Header, Body),
    case gun:await(State#state.conn, StreamRef, 3000, State#state.conn_mref) of
        {response, nofin, 200, _} -> gun:await_body(State#state.conn, StreamRef, 1000, State#state.conn_mref);
        {response, _, 400, _} -> {error, badrequest};
        {response, _, 403, _} -> {error, badauth};
        {response, _, 404, _} -> {error, notfound};
        {response, _, 413, _} -> {error, sizelimit};
        {response, _, 429, _} -> {error, toomanyrequests};
        {response, _, 456, _} -> {error, quotaexceeded};
        {response, _, 503, _} -> {error, resourceunavailable};
        {response, _, Status, _} -> {error, {deeplerror, Status}};
        {error, _} = Error -> Error
    end.
