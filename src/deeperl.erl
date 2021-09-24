-module(deeperl).
-behaviour(gen_server).

%% API
-export([start_link/0, auth_key/1, usage/0, languages/0, translate/2, translate/3]).
-export_type([translation_options/0, usage_result/0, languages_result/0, translate_result/0]).

-type language() :: nonempty_string().
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

-type usage_result() :: {CharacterCount :: pos_integer(), CharacterLimit :: pos_integer()} | {error, invalid_response} | {error, term()}.
-type languages_result() :: [{Language :: deeperl:language(), Name :: binary(), SupportsFormality :: boolean()}] | {error, invalid_response} | {error, term()}.
-type translate_result() :: [{DetectedSourceLanguage :: deeperl:language(), Text :: iodata()}] | {error, invalid_response} | {error, {bad_option, Option :: any()}} | {error, term()}.


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

%% @private
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
init(_) ->
    GunOpts = case application:get_env(gun_opts) of
                  undefined -> #{};
                  {ok, Opts} -> Opts
              end,
    {ok, ConnPid} = gun:open("api.deepl.com", 443, GunOpts),
    {ok, #state{
        conn = ConnPid,
        conn_mref = monitor(process, ConnPid)
    }}.

%% @private
handle_call(usage, _From, State) ->
    {reply, call(deeperl_usage, [], State), State};
handle_call(languages, _From, State) ->
    {reply, call(deeperl_languages, [], State), State};
handle_call({translate, TargetLang, Texts, Options}, _From, State) ->
    {reply, call(deeperl_translate, {TargetLang, Texts, Options}, State), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', Mref, process, _ConnPid, Reason}, #state{conn_mref = Mref}) ->
    exit(Reason);
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec usage() -> usage_result().
usage() ->
    gen_server:call(?SERVER, usage).

-spec languages() -> languages_result().
languages() ->
    gen_server:call(?SERVER, languages).

-spec translate(
    TargetLang :: language(),
    Texts :: [iodata()]
) -> translate_result().
translate(TargetLang, Texts) ->
    translate(TargetLang, Texts, #{}).

-spec translate(
    TargetLang :: language(),
    Text :: [iodata()],
    Options :: translation_options()
) -> translate_result().
translate(TargetLang, Texts, Options) ->
    gen_server:call(?SERVER, {translate, TargetLang, Texts, Options}).

-spec auth_key(AuthKey :: nonempty_string()) -> ok.
auth_key(AuthKey) ->
    application:set_env(deeperl, auth_key, AuthKey).

%%%===================================================================
%%% Internal functions
%%%===================================================================

call(Module, Args, State) ->
    Request = case application:get_env(auth_key) of
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

request(Uri, Header, Body, #state{conn = ConnPid, conn_mref = MRef}) ->
    StreamRef = gun:post(ConnPid, Uri, Header, Body),
    case gun:await(ConnPid, StreamRef, 3000, MRef) of
        {response, nofin, 200, _} -> gun:await_body(ConnPid, StreamRef, 1000, MRef);
        {response, fin, 200, _} -> {error, emptyresponse};
        {response, IsFin, 400, _} -> error_message(IsFin, bad_request, ConnPid, StreamRef, MRef);
        {response, IsFin, 403, _} -> error_message(IsFin, bad_auth, ConnPid, StreamRef, MRef);
        {response, IsFin, 404, _} -> error_message(IsFin, not_found, ConnPid, StreamRef, MRef);
        {response, IsFin, 413, _} -> error_message(IsFin, size_limit, ConnPid, StreamRef, MRef);
        {response, IsFin, 429, _} -> error_message(IsFin, too_many_requests, ConnPid, StreamRef, MRef);
        {response, IsFin, 456, _} -> error_message(IsFin, quota_exceeded, ConnPid, StreamRef, MRef);
        {response, IsFin, 503, _} -> error_message(IsFin, temporarily_unavailable, ConnPid, StreamRef, MRef);
        {response, IsFin, _, _} -> error_message(IsFin, unknown_error, ConnPid, StreamRef, MRef);
        {error, _} = Error -> Error
    end.

error_message(fin, Error, _ConnPid, _StreamRef, _MRef) ->
    {error, Error};
error_message(nofin, Error, ConnPid, StreamRef, MRef) ->
    {ok, Body} = gun:await_body(ConnPid, StreamRef, 1000, MRef),
    {error, {Error, Body}}.
