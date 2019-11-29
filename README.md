# deeperl

An Erlang/OTP application for the official [DeepL  API].

**HIGHLY EXPERIMENTAL** at the moment, so use at your own risk.

[![Hex pm](http://img.shields.io/hexpm/v/deeperl.svg?style=flat)](https://hex.pm/packages/deeperl)

## Configuration

### Authentication
The DeepL authentication key can be set in the application environment using the `auth_key` setting or using `deeperl:auth_key/1`.

Changing the authentication key at runtime will take effect immediately.


### Http Options
deeperl utilizes [Gun] in the default configuration for its http requests. Gun options can be set in the application environment with the `gun_opts` setting.

Changing the Gun options will take effect after restarting deeperl.

## Starting deeperl
deeperl is an [OTP] application. Before using any of the functions you will have to start it.

To start in the console run:
```sh
$ rebar3 shell
```
```erlang
1> application:ensure_all_started(deeperl).
{ok,[cowlib,gun,deeperl]}
```
Or add deeperl to the applications property of your .app files.

## Basic usage
### Monitoring usage
Official API documentation: [https://www.deepl.com/docs-api.html?part=other#other__usage]
```erlang
1> {CharacterCount, CharacterLimit} = deeperl:usage().
{23586,50000000}
````

### Listing supported languages
Official API documentation: [https://www.deepl.com/docs-api.html?part=other#other__languages]
```erlang
2> Languages = deeperl:languages().
[{en,<<"English">>},
 {de,<<"German">>},
 {fr,<<"French">>},
 {es,<<"Spanish">>},
 {pt,<<"Portuguese">>},
 {it,<<"Italian">>},
 {nl,<<"Dutch">>},
 {pl,<<"Polish">>},
 {ru,<<"Russian">>}]
````

### Translating text
Official API documentation: [https://www.deepl.com/docs-api.html?part=translating_text]

```erlang
3> TargetLanguage = it,
3> Texts = ["Dear sir or madam", "This is an example"],
3> deeperl:translate(TargetLanguage, Texts).
[{en,<<"Gentile signore o signora">>},
 {en,<<"Questo è un esempio"/utf8>>}]
```

Translation options can be set with `deeperl:translate/3`:
```erlang
4> Options = #{preserve_formatting => true, split_sentences => nonewlines},
4> deeperl:translate(TargetLanguage, Texts, Options).
[{en,<<"Gentile signore o signora">>},
 {en,<<"Questo è un esempio"/utf8>>}]
```
The purpose of each option is described in the API documentation of DeepL.

The type specification for the options are:
```erlang
-type translation_options() :: #{
    source_lang => de | en | es | fr | it | nl | pl | pt | ru,
    split_sentences => boolean() | nonewlines,
    preserve_formatting => boolean(),
    tag_handling => xml,
    outline_detection => boolean(),
    non_splitting_tags =>  [nonempty_string()],
    splitting_tags => [nonempty_string()],
    ignore_tags => [nonempty_string()]
}.
````

[DeepL  API]: https://www.deepl.com/docs-api.html
[Gun]: https://ninenines.eu/docs/en/gun/2.0/guide/
[hex]: https://hex.pm/packages/deeperl
[OTP]: http://www.erlang.org/doc/design_principles/users_guide.html
