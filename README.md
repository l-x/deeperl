[![Hex pm](https://img.shields.io/hexpm/l/deeperl.svg?style=flat)](https://hex.pm/packages/deeperl)
[![Hex pm](https://img.shields.io/hexpm/v/deeperl.svg?style=flat)](https://hex.pm/packages/deeperl)


# deeperl

An Erlang/OTP client application for the official [DeepL  API].

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

The DeepL authentication key can be set in the application environment using the `auth_key` setting or at runtime: 

```erlang
2> deeperl:auth_key("your DeepL auth_key here").
ok.
```
Changing the authentication key at runtime will take effect immediately.

**Note**: deeperl recognizes which API endpoint (api-free.deepl.com or api.deepl.com) to use based on the token. 
### Translating Text

### Glossaries

#### Listing all glossaries

#### Getting detailed information for a glossary

#### Getting entries for a glossary

### Other functions

#### Getting the list of all supported source languages

#### Getting the list of all supported target languages

#### Getting your account's usage metrics


[DeepL  API]: https://www.deepl.com/docs-api.html
[hex]: https://hex.pm/packages/deeperl