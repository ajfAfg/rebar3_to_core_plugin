# rebar3_to_core_plugin

A rebar plugin for compiling into Core Erlang

## Build

```bash
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{plugins, [
    {rebar3_to_core_plugin, {git, "https://github.com/ajfAfg/rebar3_to_core_plugin.git", {branch, "main"}}}
]}.
```

Then just call your plugin directly in an existing application:

```bash
$ rebar3 to_core
===> Fetching rebar3_to_core_plugin
===> Compiling rebar3_to_core_plugin
<Plugin Output>
```
