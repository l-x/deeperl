-module(deeperl).
-behaviour(gen_server).

-type language() :: nonempty_list().

-type glossary_id() :: nonempty_list().

-type glossary_name() :: iolist().

-type glossary() :: #{
    glossary_id => glossary_id(), 
    name => glossary_name(),
    source_lang => language(),
    target_lang => language(),
    creation_time => nonempty_list(),
    entry_count => integer()
}.

-type glossary_entry() :: {iolist(), iolist()}.
-type glossary_entries() :: [glossary_entry()].

-type tag() :: iolist().
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

-export_types([
    language/0,

    glossary_id/0,
    glossary_name/0,
    glossary/0,
    glossary_entry/0,
    glossary_entries/0,

    tag/0,
    tag_list/0,
    translation_options/0
]).

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

-spec auth_key(AuthKey :: nonempty_list()) -> 
    ok.
auth_key(AuthKey) ->
    application:set_env(deeperl, auth_key, AuthKey).

-spec glossary_list() -> {ok, [glossary()]}.
glossary_list() ->
    gen_server:call(?SERVER, {list_glossaries}).

-spec glossary_information(GlossaryId :: glossary_id()) -> {ok, glossary()}.
glossary_information(GlossaryId) ->
    gen_server:call(?SERVER, {glossary_information, GlossaryId}).

-spec glossary_entries(GlossaryId :: glossary_id()) -> {ok, glossary_entries()}.
glossary_entries(GlossaryId) ->
    gen_server:call(?SERVER, {glossary_entries, GlossaryId}).

-spec glossary_delete(GlossaryId :: glossary_id()) -> ok.
glossary_delete(GlossaryId) ->
    gen_server:call(?SERVER, {delete_glossary, GlossaryId}).

-spec glossary_create(
        Name :: glossary_name(), 
        SourceLang :: language(), 
        TargetLang :: language(), 
        Entries :: glossary_entries()
    ) -> 
        {ok, glossary()}.
glossary_create(Name, SourceLang, TargetLang, Entries) ->
    gen_server:call(?SERVER, {create_glossary, Name, SourceLang, TargetLang, Entries}).

-spec translate(TargetLang :: language(), Texts :: [iolist()]) -> 
    {ok, [{DetectedLanguage :: language(), Text :: iolist()}]}.
translate(TargetLang, Texts) ->
    translate(TargetLang, Texts, #{}).

-spec translate(TargetLang :: language(), Texts :: [iolist()], Options :: translation_options()) -> 
    {ok, [{DetectedLanguage :: language(), Text :: iolist()}]}.
translate(TargetLang, Texts, #{} = Options) ->
    gen_server:call(?SERVER, {translate, TargetLang, Texts, Options}).

-spec source_languages() -> {ok, [{Language :: language(), Name :: iolist()}]}.
source_languages() ->
    gen_server:call(?SERVER, {source_languages}).

-spec target_languages() -> {ok, [{Language :: language(), Name :: iolist(), SupportsFormality :: boolean()}]}.
target_languages() ->
    gen_server:call(?SERVER, {target_languages}).

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
    {ok, AuthKey} = auth_key(),

    {reply, deeperl_client:call(AuthKey, deeperl_glossary:list()), State};

handle_call({glossary_information, GlossaryId}, _From, State) ->
    {ok, AuthKey} = auth_key(),

    {reply, deeperl_client:call(AuthKey, deeperl_glossary:information(GlossaryId)), State};

handle_call({glossary_entries, GlossaryId}, _From, State) ->
    {ok, AuthKey} = auth_key(),

    {reply, deeperl_client:call(AuthKey, deeperl_glossary:entries(GlossaryId)), State};

handle_call({delete_glossary, GlossaryId}, _From, State) ->
    {ok, AuthKey} = auth_key(),

    {reply, deeperl_client:call(AuthKey, deeperl_glossary:delete(GlossaryId)), State};

handle_call({create_glossary, Name, SourceLang, TargetLang, Entries}, _From, State) ->
    {ok, AuthKey} = auth_key(),
    FunctionConfig = deeperl_glossary:create(
        Name,
        SourceLang,
        TargetLang,
        Entries
    ),

    {reply, deeperl_client:call(AuthKey, FunctionConfig), State};


handle_call({translate, TargetLang, Texts, #{} = Options}, _From, State) ->
    {ok, AuthKey} = auth_key(),

    {reply, deeperl_client:call(AuthKey, deeperl_translation:translate(TargetLang, Texts, Options)), State};

handle_call({source_languages}, _From, State) ->
    {ok, AuthKey} = auth_key(),

    {reply, deeperl_client:call(AuthKey, deeperl_translation:source_languages()), State};

handle_call({target_languages}, _From, State) ->
    {ok, AuthKey} = auth_key(),

    {reply, deeperl_client:call(AuthKey, deeperl_translation:target_languages()), State};

handle_call({usage}, _From, State) ->
    {ok, AuthKey} = auth_key(),

    {reply, deeperl_client:call(AuthKey, deeperl_translation:usage()), State};

%% @private
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

-spec auth_key() -> 
    {ok, nonempty_list()} | undefined.
auth_key() ->
    application:get_env(deeperl, auth_key).