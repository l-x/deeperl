[![Hex pm](https://img.shields.io/hexpm/l/deeperl.svg?style=flat)](https://hex.pm/packages/deeperl)
[![Hex pm](https://img.shields.io/hexpm/v/deeperl.svg?style=flat)](https://hex.pm/packages/deeperl)


# deeperl

An Erlang/OTP client application for the official [DeepL  API] Version 2.

**HIGHLY EXPERIMENTAL** and work in progress atm, so use at your own risk.

## Installation

The preferred way of installing deeperl is via [hex].

## Usage

### Starting deeperl

deeperl is an Erlang/OTP application. Before using any of the functions you will have to start it.

To start in the console run:
```sh
$ rebar3 shell
```
```erlang
1> application:ensure_all_started(deeperl).
{ok,[inets,deeperl]}
```
Or add deeperl to the applications property of your .app files.

### Authentication
> [Official API documentation](https://www.deepl.com/docs-api/accessing-the-api/authentication/)

The DeepL authentication key can be set in the application environment using the `auth_key` setting or at runtime: 

```erlang
2> ok = deeperl:auth_key("your DeepL auth_key here").
ok.
```
Changing the authentication key at runtime will take effect immediately.

**Note**: deeperl recognizes which API endpoint (api-free.deepl.com or api.deepl.com) to use based on the token. 
### Translating Text
> [Official API documentation](https://www.deepl.com/docs-api/translating-text/)


### Translating documents
> [Official API documentation](https://www.deepl.com/docs-api/translating-documents/)

Translating documents is not supported yet.

### Managing glossaries
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/)

#### Creating a glossary
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/creating-a-glossary/)

```erlang
4> Entries = [{<<Entry1>>, <<"Translation1">>}, {<<Entry2>>, <<"Translation2">>}, {<<Entry3>>, <<"Translation3">>}].
[{<<"Entry1">>,<<"Translation1">>},
 {<<"Entry2">>,<<"Translation2">>},
 {<<"Entry3">>,<<"Translation3">>}]

5> {ok, Glossary} = deeperl:glossary_create(<<"Glossary Name">>, "en", "de", Entries).
{ok,#{creation_time => "2021-09-29T07:31:01.19704+00:00",
      entry_count => 3,
      id => "610a3145-be30-424c-8eeb-bb06a405c90e",
      name => <<"Glossary Name">>,source_lang => "en",
      target_lang => "de"}}
```

#### Listing glossaries
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/listing-glossaries/)

```erlang
6> {ok, Glossaries} = deeperl:glossary_list().
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

#### Listing glossary information
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/listing-glossary-information/)

```erlang
7> {ok, Glossary} = deeperl:glossary_information("610a3145-be30-424c-8eeb-bb06a405c90e").
{ok,#{creation_time => "2021-09-29T07:31:01.19704+00:00",
      entry_count => 3,
      id => "610a3145-be30-424c-8eeb-bb06a405c90e",
      name => <<"Glossary Name">>,source_lang => "en",
      target_lang => "de"}}
```

#### Listing entries of a glossary
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/listing-entries-of-a-glossary/)

```erlang
1> {ok, Entries} = deeperl:glossary_entries("610a3145-be30-424c-8eeb-bb06a405c90e").
{ok,{<<"Entry1">>,<<"Translation1">>},
    {<<"Entry2">>,<<"Translation2">>},
    {<<"Entry3">>,<<"Translation3">>}]}
```

#### Deleting a glossary
> [Official API documentation](https://www.deepl.com/docs-api/managing-glossaries/deleing-a-glossary/)

```erlang
1> ok = deeperl:glossary_delete("610a3145-be30-424c-8eeb-bb06a405c90e").
ok
```

### Other functions

#### Listing supported languages
> [Official API documentation](https://www.deepl.com/docs-api/other-functions/listing-supported-languages/)

##### Supported source languages
```erlang
1> {ok, SourceLanguages} = deeperl:source_languages().
{ok,[{"BG","Bulgarian"},
     {"CS","Czech"},
     {"DA","Danish"},
     {"DE","German"},
     ...
```

##### Supported target languages
```erlang
1> {ok, TargetLanguages} = deeperl:target_languages().
{ok,[{"BG","Bulgarian",false},
     {"CS","Czech",false},
     {"DA","Danish",false},
     {"DE","German",true},
     ...
```

#### Monitoring usage
> [Official API documentation](https://www.deepl.com/docs-api/other-functions/monitoring-usage/)

```erlang
2> {ok, {CharacterCount, CharacterLimit}} = deeperl:usage().
{ok,{28788,50000000}}
```

[DeepL  API]: https://www.deepl.com/de/docs-api/
[hex]: https://hex.pm/packages/deeperl