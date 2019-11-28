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
    {reply, call(deeperl_translate, [TargetLang, Texts, Options], State), State};
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

%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Get the DeepL usage statistics
%% @end
%% ---------------------------------------------------------------------------------------------------------------------
-spec usage() -> {CharacterCount :: non_neg_integer(), CharacterLimit :: non_neg_integer()} | {error, Reason :: term()}.
usage() ->
    gen_server:call(?SERVER, usage).

%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Get the languages currently supported by DeepL
%% @end
%% ---------------------------------------------------------------------------------------------------------------------
-spec languages() -> [{Language :: language(), Text :: nonempty_string()}] | {error, Reason :: term()}.
languages() ->
    gen_server:call(?SERVER, languages).

%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Translate Text
%% @end
%% ---------------------------------------------------------------------------------------------------------------------
-spec translate(
    TargetLang :: language(),
    Text :: nonempty_string() | [nonempty_string()]
) -> [{DetectedSourceLanguage :: language(), Text :: nonempty_string()}] | {error, Reason :: term()}.
translate(TargetLang, Texts) ->
    translate(TargetLang, Texts, #{}).

%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Translate Text with additional options
%% @end
%% ---------------------------------------------------------------------------------------------------------------------
-spec translate(
    TargetLang :: language(),
    Text :: nonempty_string() | [nonempty_string()],
    Options :: translation_options()
) -> [{DetectedSourceLanguage :: language(), Text :: nonempty_string()}] | {error, Reason :: term()}.
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

request(Uri, Header, Body, #state{conn =  ConnPid, conn_mref = MRef} = State) ->
    StreamRef = gun:post(State#state.conn, Uri, Header, Body),
    case gun:await(State#state.conn, StreamRef, 3000, State#state.conn_mref) of
        {response, nofin, 200, _} -> gun:await_body(ConnPid, StreamRef, 1000, MRef);
        {response, fin, 200, _} -> {error, emptyresponse};
        {response, IsFin, 400, _} -> error_message(IsFin, badrequest, ConnPid, StreamRef, MRef);
        {response, IsFin, 403, _} -> error_message(IsFin, badauth, ConnPid, StreamRef, MRef);
        {response, IsFin, 404, _} -> error_message(IsFin, notfound, ConnPid, StreamRef, MRef);
        {response, IsFin, 413, _} -> error_message(IsFin, sizelimit, ConnPid, StreamRef, MRef);
        {response, IsFin, 429, _} -> error_message(IsFin, 'too many requests', ConnPid, StreamRef, MRef);
        {response, IsFin, 456, _} -> error_message(IsFin, 'quota exceeded', ConnPid, StreamRef, MRef);
        {response, IsFin, 503, _} -> error_message(IsFin, 'resource temporarily unavailable', ConnPid, StreamRef, MRef);
        {response, IsFin, _, _} -> error_message(IsFin, 'unknown error', ConnPid, StreamRef, MRef);
        {error, _} = Error -> Error
    end.

error_message(fin, Error, _ConnPid, _StreamRef, _MRef) ->
    {error, Error};
error_message(nofin, Error, ConnPid, StreamRef, MRef) ->
    {ok, Body} = gun:await_body(ConnPid, StreamRef, 1000, MRef),
    {error, {Error, jiffy:decode(Body, [return_maps])}}.
