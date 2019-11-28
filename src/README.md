# deeperl

An Erlang/OTP application for the [DeepL  API].

## Basic usage

### Start
```
$ rebar3 shell
```
```erlang
1> application:set_env([{deeperl, [{authkey, "YOUR_DEEPL_API_KEY"}]}]).
ok
2> deeperl:usage().
{12901,50000000}
3> deeperl:languages().
[{en,"English"},
 {de,"German"},
 {fr,"French"},
 {es,"Spanish"},
 {pt,"Portuguese"},
 {it,"Italian"},
 {nl,"Dutch"},
 {pl,"Polish"},
 {ru,"Russian"}]
4> deeperl:translate(de, "Hello world").
[{en,"Hallo Welt"}]
5> 
```

[DeepL  API]: https://www.deepl.com/docs-api.html
