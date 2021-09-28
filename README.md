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
> https://www.deepl.com/docs-api/accessing-the-api/authentication/

The DeepL authentication key can be set in the application environment using the `auth_key` setting or at runtime: 

```erlang
2> deeperl:auth_key("your DeepL auth_key here").
ok.
```
Changing the authentication key at runtime will take effect immediately.

**Note**: deeperl recognizes which API endpoint (api-free.deepl.com or api.deepl.com) to use based on the token. 
### Translating Text
> https://www.deepl.com/docs-api/translating-text/


### Translating documents
> https://www.deepl.com/docs-api/translating-documents/

Translating documents is not supported yet.

### Managing glossaries
> https://www.deepl.com/docs-api/managing-glossaries/

#### Creating a glossary
> https://www.deepl.com/docs-api/managing-glossaries/creating-a-glossary/

#### Listing glossaries
> https://www.deepl.com/docs-api/managing-glossaries/listing-glossaries/

#### Listing glossary information
> https://www.deepl.com/docs-api/managing-glossaries/listing-glossary-information/

#### Listing entries of a glossary
> https://www.deepl.com/docs-api/managing-glossaries/listing-entries-of-a-glossary/

#### Deleting a glossary
> https://www.deepl.com/docs-api/managing-glossaries/deleing-a-glossary/

### Other functions

#### Listing supported languages
> https://www.deepl.com/docs-api/other-functions/listing-supported-languages/

#### Monitoring usage
> https://www.deepl.com/docs-api/other-functions/monitoring-usage/

[DeepL  API]: https://www.deepl.com/de/docs-api/
[hex]: https://hex.pm/packages/deeperl