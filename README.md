[![Codeberg](https://img.shields.io/badge/Codeberg-deeperl-green?logo=codeberg&style=for-the-badge)](https://codeberg.org/l-x/deeperl)
[![Hex pm](https://img.shields.io/hexpm/v/deeperl.svg?style=for-the-badge&)](https://hex.pm/packages/deeperl)
[![Hex pm](https://img.shields.io/hexpm/l/deeperl.svg?style=for-the-badge&)](https://hex.pm/packages/deeperl)

# deeperl

An Erlang/OTP client application for the official [DeepL  API] Version 2.

## Installation

### Rebar3
```erlang
{deps, [
  % Latest release (git):
  {deeperl, {git, "https://codeberg.org/l-x/deeperl.git", {branch, "main"}}},
  
  % Specific version (git):
  {deeperl, {git, "https://codeberg.org/l-x/deeperl.git", {tag, "0.10.0"}}},

  % Latest version (hex):
  deeperl,

  % Specific version (hex):
  {deeperl, "0.10.0"}
]}.
``` 

### erlang.mk
See https://erlang.mk/guide/deps.html

## Usage

### Starting deeperl

deeperl is an Erlang/OTP application. Before using any of the functions you will have to start it.

To start in the console run:
```sh
$ rebar3 shell
```
Or simply add deeperl to the applications property of your .app files.

### Authentication
To use the DeepL API an authentication key is required. 

When booting deeperl this key is retrieved from `application:get_env(deeperl, auth_key)` and stored in the deeperl server processes internal state.

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
1> application:auth_key("your DeepL auth_key here").
ok
```

However, this does not change the corresponding application environment setting.

**Note**: deeperl determines what API endpoint to use based on the provided auth_key.

### Configuring the http client

deeperl uses the [builtin HTTP client](https://erlang.org/doc/apps/inets/http_client.html) to perform all HTTP requests.

By default deeperl uses the `default` profile for `httpc`. The used profile is stored in the application environment under the key `httpc_profile`:
`yourapp.config`
```erlang
[
  {deeperl, [
    {httpc_profile, my_profile}
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

### Translating Text

```erlang
5> {ok, Translations} = deeperl:translate("de", ["Rien ne vas plus", <<"Game over">>]).
{ok,[{"FR",<<"Nichts läuft richtig"/utf8>>},
     {"EN",<<"Spiel vorbei">>}]}

6> {ok, Translations} = deeperl:translate("de", ["<b>Rien</b> ne vas plus", <<"Game over">>], #{tag_handling=>xml, formality=>less}).
{ok,[{"FR",<<"<b>Nichts</b> läuft richtig"/utf8>>},
     {"EN",<<"Spiel vorbei">>}]}
```
### Listing supported languages

#### Supported source languages
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

#### Supported target languages
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

### Monitoring usage

```erlang
11> {ok, {CharacterCount, CharacterLimit}} = deeperl:usage().
{ok,{28788,50000000}}
```

### Managing glossaries

#### List language pairs supported by glossaries

```erlang
12> {ok, LanguagePairs} = deeperl:glossary_language_pairs().
{ok,[{"de","en"},
     {"en","de"},
     {"en","es"},
     {"en","fr"},
     {"es","en"},
     {"fr","en"}]}
```

#### Creating a glossary

```erlang
13> Entries = [{<<"Entry1">>, <<"Translation1">>}, {<<"Entry2">>, <<"Translation2">>}, {<<"Entry3">>, <<"Translation3">>}].
[{<<"Entry1">>,<<"Translation1">>},
 {<<"Entry2">>,<<"Translation2">>},
 {<<"Entry3">>,<<"Translation3">>}]

14> {ok, Glossary} = deeperl:glossary_create(<<"Glossary Name">>, "en", "de", Entries).
{ok,#{creation_time => "2021-09-29T07:31:01.19704+00:00",
      entry_count => 3,
      id => "610a3145-be30-424c-8eeb-bb06a405c90e",
      name => <<"Glossary Name">>,source_lang => "en",
      target_lang => "de"}}

15> GlossaryId = maps:get(id, Glossary).
"610a3145-be30-424c-8eeb-bb06a405c90e"
```

#### Listing glossary information

```erlang
16> {ok, Glossary} = deeperl:glossary_information(GlossaryId).
{ok,#{creation_time => "2021-09-29T07:31:01.19704+00:00",
      entry_count => 3,
      id => "610a3145-be30-424c-8eeb-bb06a405c90e",
      name => <<"Glossary Name">>,source_lang => "en",
      target_lang => "de"}}
```

#### Listing entries of a glossary

```erlang
17> {ok, _} = deeperl:glossary_entries(GlossaryId).
{ok,[{"Entry1","Translation1"},
     {"Entry2","Translation2"},
     {"Entry3","Translation3"}]}
```

#### Listing glossaries

```erlang
18> {ok, Glossaries} = deeperl:glossary_list().
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

#### Deleting a glossary

```erlang
19> ok = deeperl:glossary_delete(GlossaryId).
ok
```

[DeepL  API]: https://www.deepl.com/de/docs-api/
[hex]: https://hex.pm/packages/deeperl
