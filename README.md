rebar3_to_core_plugin
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_to_core_plugin, {git, "https://host/user/rebar3_to_core_plugin.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_to_core_plugin
    ===> Fetching rebar3_to_core_plugin
    ===> Compiling rebar3_to_core_plugin
    <Plugin Output>
