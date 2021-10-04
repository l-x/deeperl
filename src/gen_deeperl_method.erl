%% @private
-module(gen_deeperl_method).

-type short_request() :: {
    Method :: get | delete,
    Data :: {
        Path :: nonempty_string(),
        Headers :: list({nonempty_string(), nonempty_string()})
    }
}.

-type full_request() :: {
    Method :: post,
    Data :: {
        Path :: nonempty_string(),
        Headers :: list({nonempty_string(), nonempty_string()}),
        ContentType :: nonempty_string(),
        Body :: string() | binary()
    }
}.

-type request() :: short_request() | full_request().

%% API
-export([]).

-callback request(Options :: tuple()) -> request().
-callback response(Body :: string() | binary()) -> {ok, term()} | ok.
