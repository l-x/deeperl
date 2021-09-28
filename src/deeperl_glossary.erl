%%% @private

-module(deeperl_glossary).

-export([
    list/0,
    create/4,
    information/1,
    entries/1,
    delete/1    
]).

glossary(#{} = ResultMap) ->
    F = fun (K, V, Map) when is_binary(V) -> maps:put(binary_to_list(K), binary_to_list(V), Map);
            (K, V, Map) -> maps:put(binary_to_list(K), V, Map) end,

    maps:fold(F, #{}, ResultMap).

entries(GlossaryId) ->
    {
        {
            get,
            {"/v2/glossaries/" ++ GlossaryId ++ "/entries", []}
        },
        fun
            (200, ResponseBody) ->
                Result = [list_to_tuple(string:split(L, "\t")) || L <-string:split(ResponseBody, "\n")],
                {ok, Result};
            (404, _) ->
                {error, not_found, GlossaryId}
        end
    }.

information(GlossaryId) ->
    {
        {
            get,
            {"/v2/glossaries/" ++ GlossaryId, []}
        },
        fun
            (200, ResponseBody) ->
                Result = jiffy:decode(ResponseBody, [return_maps]),
                {ok, glossary(Result)};
            (404, _) ->
                {error, not_found, GlossaryId}
        end
    }.

list() ->
    {
        {
            get,
            {"/v2/glossaries", []}
        },
        fun(200, ResponseBody) ->
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
        fun
            (204, _) ->
                ok;
            (404, _) ->
                {error, not_found, GlossaryId}
        end
    }.

create(Name, SourceLang, TargetLang, Entries) ->
    Params = [
        {"name", Name},
        {"source_lang", SourceLang},
        {"target_lang", TargetLang},
        {"entries", string:join([From ++ "\t" ++ To || {From, To} <- Entries], "\n")},
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
        fun(201, ResponseBody) ->
            Result = jiffy:decode(ResponseBody, [return_maps]),
            {ok, glossary(Result)}
        end
    }.