%%% @private
-module(deeperl_glossary).

-export([
    create/1
]).

create(#{
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
