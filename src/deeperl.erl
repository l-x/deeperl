-module(deeperl).

%% API
-export([]).
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
