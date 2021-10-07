-module(deeperl).
-behaviour(gen_server).

-type error() :: {error, Reason :: atom(), Info :: term()}.

-type nonempty_str_or_binary() :: nonempty_string() | nonempty_binary().

-type language() :: nonempty_string().

-type glossary_id() :: nonempty_string().

-type glossary_name() :: nonempty_str_or_binary().

-type glossary() :: #{
    glossary_id => glossary_id(),
    name => glossary_name(),
    source_lang => language(),
    target_lang => language(),
    creation_time => nonempty_string(),
    entry_count => integer()
}.

-type glossary_entry() :: {nonempty_str_or_binary(), nonempty_str_or_binary()}.
-type glossary_entries() :: [glossary_entry()].

-type tag() :: nonempty_str_or_binary().
-type tag_list() :: [tag()].

-type translation_options() :: #{
    source_lang => language(),
    split_sentences => boolean() | nonewlines,
    preserve_formatting => boolean(),
    tag_handling => xml,
    non_splitting_tags => tag_list(),
    splitting_tags => tag_list(),
    ignore_tags => tag_list(),
    outline_detection => boolean(),
    formality => default | more | less,
    glossary_id => glossary_id()
}.

%% API

-export([
    start_link/1,

    auth_key/1,
    httpc_profile/1,

    glossary_list/0,
    glossary_information/1,
    glossary_entries/1,
    glossary_delete/1,
    glossary_create/4,

    translate/2,
    translate/3,

    source_languages/0,
    target_languages/0,

    usage/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    auth_key :: string(),
    httpc_profile :: atom() | pid()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Set the DeepL authentication key
-spec auth_key(AuthKey :: nonempty_string()) -> ok.
auth_key(AuthKey) ->
    gen_server:cast(?SERVER, {auth_key, AuthKey}).

%% @doc Set the inets httpc profile to be used
-spec httpc_profile(Profile :: pid() | atom()) -> ok.
httpc_profile(Profile) ->
    gen_server:cast(?SERVER, {httpc_profile, Profile}).

%% @doc List all glossaries
-spec glossary_list() -> {ok, [glossary()]} | error().
glossary_list() ->
    gen_server:call(?SERVER, {list_glossaries}).

%% @doc Get detailed information for a specific glossary
-spec glossary_information(GlossaryId :: glossary_id()) -> {ok, glossary()} | error().
glossary_information(GlossaryId) ->
    gen_server:call(?SERVER, {glossary_information, GlossaryId}).

%% @doc Get the entries for a specific glossary
-spec glossary_entries(GlossaryId :: glossary_id()) -> {ok, glossary_entries()} | error().
glossary_entries(GlossaryId) ->
    gen_server:call(?SERVER, {glossary_entries, GlossaryId}).

%% @doc Delete a glossary
-spec glossary_delete(GlossaryId :: glossary_id()) -> ok | error().
glossary_delete(GlossaryId) ->
    gen_server:call(?SERVER, {delete_glossary, GlossaryId}).

%% @doc Create a glossary
-spec glossary_create(
    Name :: glossary_name(),
    SourceLang :: language(),
    TargetLang :: language(),
    Entries :: glossary_entries()
) ->
    {ok, glossary()} | error().
glossary_create(Name, SourceLang, TargetLang, Entries) ->
    gen_server:call(?SERVER, {create_glossary, Name, SourceLang, TargetLang, Entries}).

%% @doc Translate a list of texts
-spec translate(TargetLang :: language(), Texts :: [nonempty_str_or_binary()]) ->
    {ok, [{DetectedLanguage :: language(), Text :: nonempty_str_or_binary()}]} | error().
translate(TargetLang, Texts) ->
    translate(TargetLang, Texts, #{}).

%% @doc Translate a list of texts with special translation options
-spec translate(TargetLang :: language(), Texts :: [nonempty_str_or_binary()], Options :: translation_options()) ->
    {ok, [{DetectedLanguage :: language(), Text :: nonempty_str_or_binary()}]} | error().
translate(TargetLang, Texts, #{} = Options) ->
    gen_server:call(?SERVER, {translate, TargetLang, Texts, Options}).

%% @doc Get the list of languages DeepL can translate from
-spec source_languages() -> {ok, [{Language :: language(), Name :: nonempty_str_or_binary()}]} | error().
source_languages() ->
    gen_server:call(?SERVER, {source_languages}).

%% @doc Get the list of languages DeepL can translate to
-spec target_languages() -> {ok, [{Language :: language(), Name :: nonempty_str_or_binary(), SupportsFormality :: boolean()}]} | error().
target_languages() ->
    gen_server:call(?SERVER, {target_languages}).

%% @doc See the usage statistics for you account
-spec usage() -> {ok, {CharacterCount :: integer(), CharacterLimit :: integer()}} | error().
usage() ->
    gen_server:call(?SERVER, {usage}).

%% @private
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% @private
init(#{auth_key := AuthKey, httpc_profile := HttpcProfile}) ->
    {ok, #state{
        auth_key = AuthKey,
        httpc_profile = HttpcProfile
    }}.

%% @private
handle_call({list_glossaries}, _From, State) ->
    {reply, deeperl_client:call(config(State), {deeperl_glossary_list, {}}), State};

handle_call({glossary_information, GlossaryId}, _From, State) ->
    {reply, deeperl_client:call(config(State), {deeperl_glossary_information, {GlossaryId}}), State};

handle_call({glossary_entries, GlossaryId}, _From, State) ->
    {reply, deeperl_client:call(config(State), {deeperl_glossary_entries, {GlossaryId}}), State};

handle_call({delete_glossary, GlossaryId}, _From, State) ->
    {reply, deeperl_client:call(config(State), {deeperl_glossary_delete, {GlossaryId}}), State};

handle_call({create_glossary, Name, SourceLang, TargetLang, Entries}, _From, State) ->
    {reply, deeperl_client:call(config(State), {deeperl_glossary_create, {Name, SourceLang, TargetLang, Entries}}), State};

handle_call({translate, TargetLang, Texts, #{} = Options}, _From, State) ->
    {reply, deeperl_client:call(config(State), {deeperl_translate, {TargetLang, Texts, Options}}), State};

handle_call({source_languages}, _From, State) ->
    {reply, deeperl_client:call(config(State), {deeperl_source_languages, {}}), State};

handle_call({target_languages}, _From, State) ->
    {reply, deeperl_client:call(config(State), {deeperl_target_languages, {}}), State};

handle_call({usage}, _From, State) ->
    {reply, deeperl_client:call(config(State), {deeperl_usage, {}}), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast({auth_key, AuthKey}, State) ->
    {noreply, State#state{auth_key = AuthKey}};

handle_cast({httpc_profile, Profile}, State) ->
    {noreply, State#state{httpc_profile = Profile}};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
config(#state{auth_key = AuthKey, httpc_profile = HttpcProfile}) ->
    {
        HttpcProfile,
        AuthKey
    }.
