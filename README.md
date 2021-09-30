[![Hex pm](https://img.shields.io/hexpm/l/deeperl.svg?style=flat)](https://hex.pm/packages/deeperl)
[![Hex pm](https://img.shields.io/hexpm/v/deeperl.svg?style=flat)](https://hex.pm/packages/deeperl)

**WORK IN PROGRESS** use at your own risk!

# deeperl

An Erlang/OTP client application for the official [DeepL  API] Version 2.

## TOC
1. [Installation](#install)
2. [Starting deeperl](#starting)
3. [Authentication](#authentication)
4. [Configuring the http client](#httpclient)
5. [Translating Text](#translating)
6. [Listing supported languages](#languages)
    1. [Supported source languages](#source_languages)
    2. [Supported target languages](#target_languages)
7. Monitoring usage [](#monitoring)
8. [Managing glossaries](#glossaries)
    1. [Creating a glossary](#glossary_create)
    2. [Listing glossary information](#glossary_information)
    3. [Listing entries of a glossary](#glossary_entries)
    4. [Listing glossaries](#glossary_list)
    5. [Deleting a glossary](#glossary_delete)

## Installation [](#install)

### Rebar3
```erlang
{deps, [
  % Latest version (git):
  {deeperl, {git, "https://codeberg.org/l-x/deeperl.git", {branch, "main"}}},
  
  % Specific version (git):
  {deeperl, {git, "https://codeberg.org/l-x/deeperl.git", {tag, "0.6.0"}}},

  % Latest version (hex):
  deeperl,

  % Specific version (hex):
  {deeperl, "0.6.0"}
]}.
``` 

### erlang.mk
See https://erlang.mk/guide/deps.html

## Usage

### Starting deeperl [](#starting)

deeperl is an Erlang/OTP application. Before using any of the functions you will have to start it.

To start in the console run:
```sh
$ rebar3 shell
```
Or simply add deeperl to the applications property of your .app files.

### Authentication [](#authentication)
> [Official API documentation](https://www.deepl.com/docs-api/accessing-the-api/authentication/)

To use the DeepL API an authentication key is required. This key is stored in the application environment and can therefore be set in your application config:

`yourapp.config`
```erlang
[
  {deeperl, [
    {auth_key, "your DeepL auth_key here"}
  ]}
].
```

The authorization key can also be set at runtime by calling
```erlang
1> application:ensure_all_started(deeperl).
{ok, [deeperl]}
```

**Note**: deeperl recognizes which API endpoint (api-free.deepl.com or api.deepl.com) to use based on the token.

### Configuring the http client [](#httpclient)

deeperl uses the [builtin HTTP client](https://erlang.org/doc/apps/inets/http_client.html) to perform all HTTP requests.

By default deeperl uses the `default` profile for `httpc`. The used profile is stored in the application environment under the key `httpc_profile`:
`yourapp.config`
```erlang
[
  {deeperl, [
    {httpc_profile, profile_name}
  ]}
].
```
The httpc profile can also be set at runtime by calling

```erlang
3> {ok, Pid} = inets:start(httpc, [{profile,my_profile}]).
{ok,<0.94.0>}

4> ok = deeperl:httpc_profile(Pid).
ok
```

### Translating Text [](#translating)
> [Official API documentation](https://www.deepl.com/docs-api/translating-text/)

```erlang
5> {ok, Translations} = deeperl:translate("de", ["Rien ne vas plus", <<"Game over">>]).
{ok,[{"FR",<<"Nichts läuft richtig"/utf8>>},
     {"EN",<<"Spiel vorbei">>}]}

6> {ok, Translations} = deeperl:translate("de", ["<b>Rien</b> ne vas plus", <<"Game over">>], #{tag_handling=>xml, formality=>less}).
{ok,[{"FR",<<"<b>Nichts</b> läuft richtig"/utf8>>},
     {"EN",<<"Spiel vorbei">>}]}
```
### Listing supported languages [](#languages)
> [Official API documentation](https://www.deepl.com/docs-api/other-functions/listing-supported-languages/)

#### Supported source languages [](#source_languages)
```erlang
7> {ok, SourceLanguages} = deeperl:source_languages().
{ok,[{"BG","Bulgarian"},
     {"CS","Czech"},
     {"DA","Danish"},
     {"DE","German"},
     ...

8> {Language, Name} = hd(SourceLanguages).
{"BG","Bulgarian"}
```

#### Supported target languages [](#target_languages)
```erlang
9> {ok, TargetLanguages} = deeperl:target_languages().
{ok,[{"BG","Bulgarian",false},
     {"CS","Czech",false},
     {"DA","Danish",false},
     {"DE","German",true},
     ...

10> {Language, Name, SupportsFormality} = hd(TargetLanguages).
{"BG","Bulgarian",false}
```

### Monitoring usage [](#monitoring)
> [Official API documentation](https://www.deepl.com/docs-api/other-functions/monitoring-usage/)
```erlang
11> {ok, {CharacterCount, CharacterLimit}} = deeperl:usage().
{ok,{28788,50000000}}
```

### Managing glossaries [](#glossaries)
#### Creating a glossary [](#glossary_create)
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/creating-a-glossary/)

```erlang
12> Entries = [{<<"Entry1">>, <<"Translation1">>}, {<<"Entry2">>, <<"Translation2">>}, {<<Entry3>>, <<"Translation3">>}].
[{<<"Entry1">>,<<"Translation1">>},
 {<<"Entry2">>,<<"Translation2">>},
 {<<"Entry3">>,<<"Translation3">>}]

13> {ok, Glossary} = deeperl:glossary_create(<<"Glossary Name">>, "en", "de", Entries).
{ok,#{creation_time => "2021-09-29T07:31:01.19704+00:00",
      entry_count => 3,
      id => "610a3145-be30-424c-8eeb-bb06a405c90e",
      name => <<"Glossary Name">>,source_lang => "en",
      target_lang => "de"}}

14> GlossaryId = maps:get(id, Glossary).
"610a3145-be30-424c-8eeb-bb06a405c90e"
```

#### Listing glossary information [](#glossary_information)
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/listing-glossary-information/)

```erlang
15> {ok, Glossary} = deeperl:glossary_information(GlossaryId).
{ok,#{creation_time => "2021-09-29T07:31:01.19704+00:00",
      entry_count => 3,
      id => "610a3145-be30-424c-8eeb-bb06a405c90e",
      name => <<"Glossary Name">>,source_lang => "en",
      target_lang => "de"}}
```

#### Listing entries of a glossary [](#glossary_entries)
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/listing-entries-of-a-glossary/)

```erlang
16> {ok, _} = deeperl:glossary_entries(GlossaryId).
{ok,[{"Entry1","Translation1"},
     {"Entry2","Translation2"},
     {"Entry3","Translation3"}]}
```

#### Listing glossaries [](#glossary_list)
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/listing-glossaries/)

```erlang
17> {ok, Glossaries} = deeperl:glossary_list().
{ok,[#{creation_time => "2021-09-29T07:31:01.19704+00:00",
      entry_count => 3,
      id => "610a3145-be30-424c-8eeb-bb06a405c90e",
      name => <<"Glossary Name">>,source_lang => "en",
      target_lang => "de"},
     #{creation_time => "2021-09-28T14:06:47.881791+00:00",
      entry_count => 1,
      id => "66a9557a-9092-4efc-901a-650a82c644ba",
      name => <<"Test3">>,source_lang => "en",target_lang => "de"},
...
```

#### Deleting a glossary [](#glossary_delete)
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/deleing-a-glossary/)

```erlang
18> ok = deeperl:glossary_delete(GlossaryId).
ok
```

[DeepL  API]: https://www.deepl.com/de/docs-api/
[hex]: https://hex.pm/packages/deeperl
