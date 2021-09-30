%%% @private
-module(deeperl_glossary).

-export([
    list/0,
    create/4,
    information/1,
    entries/1,
    delete/1
]).

glossary(#{
    <<"glossary_id">> := Id,
    <<"name">> := Name,
    <<"source_lang">> := SourceLang,
    <<"target_lang">> := TargetLang,
    <<"creation_time">> := CreationTime,
    <<"entry_count">> := EntryCount
}) ->
    #{
        id => binary_to_list(Id),
        name => Name,
        source_lang => binary_to_list(SourceLang),
        target_lang => binary_to_list(TargetLang),
        creation_time => binary_to_list(CreationTime),
        entry_count => EntryCount
    }.

entries(GlossaryId) ->
    {
        {
            get,
            {"/v2/glossaries/" ++ GlossaryId ++ "/entries", []}
        },
        fun({200, _}, ResponseBody) ->
            Lines = string:split(ResponseBody, "\n", all),

            {ok, [list_to_tuple(string:split(Line, "\t", all)) || Line <- Lines]}
        end
    }.

information(GlossaryId) ->
    {
        {
            get,
            {"/v2/glossaries/" ++ GlossaryId, []}
        },
        fun({200, _}, ResponseBody) ->
            Result = jiffy:decode(ResponseBody, [return_maps]),
            {ok, glossary(Result)}
        end
    }.

list() ->
    {
        {
            get,
            {"/v2/glossaries", []}
        },
        fun({200, _}, ResponseBody) ->
            Result = jiffy:decode(ResponseBody, [return_maps]),
            {ok, [glossary(Map) || Map <- maps:get(<<"glossaries">>, Result)]}
        end
    }.

delete(GlossaryId) ->
    {
        {
            delete,
            {"/v2/glossaries/" ++ GlossaryId, []}
        },
        fun({204, _}, _) -> ok end
    }.

create(Name, SourceLang, TargetLang, Entries) ->
    Params = [
        {"name", Name},
        {"source_lang", SourceLang},
        {"target_lang", TargetLang},
        {"entries", string:join([binary_to_list(iolist_to_binary([From, "\t", To])) || {From, To} <- Entries], "\n")},
        {"entries_format", "tsv"}
    ],
    {
        {
            post,
            {
                "/v2/glossaries",
                [],
                "application/x-www-form-urlencoded; charset=utf-8",
                uri_string:compose_query(Params)
            }
        },
        fun({201, _}, ResponseBody) ->
            Result = jiffy:decode(ResponseBody, [return_maps]),
            {ok, glossary(Result)}
        end
    }.
