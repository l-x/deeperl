-module(deeperl).
-behaviour(gen_server).

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
    start_link/0,
    auth_key/1,

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

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Set the DeepL authentication key at runtime
-spec auth_key(AuthKey :: nonempty_string()) ->
    ok.
auth_key(AuthKey) ->
    application:set_env(deeperl, auth_key, AuthKey).

%% @doc List all glossaries
-spec glossary_list() -> {ok, [glossary()]}.
glossary_list() ->
    gen_server:call(?SERVER, {list_glossaries}).

%% @doc Get detailed information for a specific glossary
-spec glossary_information(GlossaryId :: glossary_id()) -> {ok, glossary()}.
glossary_information(GlossaryId) ->
    gen_server:call(?SERVER, {glossary_information, GlossaryId}).

%% @doc Get the entries for a specific glossary
-spec glossary_entries(GlossaryId :: glossary_id()) -> {ok, glossary_entries()}.
glossary_entries(GlossaryId) ->
    gen_server:call(?SERVER, {glossary_entries, GlossaryId}).

%% @doc Delete a glossary
-spec glossary_delete(GlossaryId :: glossary_id()) -> ok.
glossary_delete(GlossaryId) ->
    gen_server:call(?SERVER, {delete_glossary, GlossaryId}).

%% @doc Create a glossary
-spec glossary_create(
    Name :: glossary_name(),
    SourceLang :: language(),
    TargetLang :: language(),
    Entries :: glossary_entries()
) ->
    {ok, glossary()}.
glossary_create(Name, SourceLang, TargetLang, Entries) ->
    gen_server:call(?SERVER, {create_glossary, Name, SourceLang, TargetLang, Entries}).

%% @doc Translate a list of texts
-spec translate(TargetLang :: language(), Texts :: [nonempty_str_or_binary()]) ->
    {ok, [{DetectedLanguage :: language(), Text :: nonempty_str_or_binary()}]}.
translate(TargetLang, Texts) ->
    translate(TargetLang, Texts, #{}).

%% @doc Translate a list of texts with special translation options
-spec translate(TargetLang :: language(), Texts :: [nonempty_str_or_binary()], Options :: translation_options()) ->
    {ok, [{DetectedLanguage :: language(), Text :: nonempty_str_or_binary()}]}.
translate(TargetLang, Texts, #{} = Options) ->
    gen_server:call(?SERVER, {translate, TargetLang, Texts, Options}).

%% @doc Get the list of languages DeepL can translate from
-spec source_languages() -> {ok, [{Language :: language(), Name :: nonempty_str_or_binary()}]}.
source_languages() ->
    gen_server:call(?SERVER, {source_languages}).

%% @doc Get the list of languages DeepL can translate to
-spec target_languages() -> {ok, [{Language :: language(), Name :: nonempty_str_or_binary(), SupportsFormality :: boolean()}]}.
target_languages() ->
    gen_server:call(?SERVER, {target_languages}).

%% @doc See the usage statistics for you account
-spec usage() -> {ok, {CharacterCount :: integer(), CharacterLimit :: integer()}}.
usage() ->
    gen_server:call(?SERVER, {usage}).

%% @private
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
init(_) ->
    {ok, #state{}}.

%% @private
handle_call({list_glossaries}, _From, State) ->
    {reply, deeperl_client:call(auth_key(), deeperl_glossary:list()), State};

handle_call({glossary_information, GlossaryId}, _From, State) ->
    {reply, deeperl_client:call(auth_key(), deeperl_glossary:information(GlossaryId)), State};

handle_call({glossary_entries, GlossaryId}, _From, State) ->
    {reply, deeperl_client:call(auth_key(), deeperl_glossary:entries(GlossaryId)), State};

handle_call({delete_glossary, GlossaryId}, _From, State) ->
    {reply, deeperl_client:call(auth_key(), deeperl_glossary:delete(GlossaryId)), State};

handle_call({create_glossary, Name, SourceLang, TargetLang, Entries}, _From, State) ->
    FunctionConfig = deeperl_glossary:create(
        Name,
        SourceLang,
        TargetLang,
        Entries
    ),

    {reply, deeperl_client:call(auth_key(), FunctionConfig), State};

handle_call({translate, TargetLang, Texts, #{} = Options}, _From, State) ->
    {reply, deeperl_client:call(auth_key(), deeperl_translation:translate(TargetLang, Texts, Options)), State};

handle_call({source_languages}, _From, State) ->
    {reply, deeperl_client:call(auth_key(), deeperl_translation:source_languages()), State};

handle_call({target_languages}, _From, State) ->
    {reply, deeperl_client:call(auth_key(), deeperl_translation:target_languages()), State};

handle_call({usage}, _From, State) ->
    {reply, deeperl_client:call(auth_key(), deeperl_translation:usage()), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
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

-spec auth_key() -> nonempty_string() | undefined.
auth_key() ->
    case application:get_env(auth_key) of
        {ok, AuthKey} -> AuthKey;
        undefined -> undefined
    end.
