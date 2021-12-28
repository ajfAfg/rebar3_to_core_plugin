-module(rebar3_to_core_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, rebar3_to_core_plugin).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, ?PROVIDER},            % The 'user friendly' name of the task
                          {module, ?MODULE},            % The module implementation of the task
                          {bare,
                           true},                 % The task can be run by the user, always true
                          {deps, ?DEPS},                % The list of dependencies
                          {example, "rebar3 rebar3_to_core_plugin"}, % How to use the plugin
                          {opts, []},                   % list of options understood by the plugin
                          {short_desc, "A rebar plugin"},
                          {desc, "A rebar plugin"}]),
    % NewState = rebar_state:set(State, erl_opts, to_core),
    % {ok, rebar_state:add_provider(NewState, Provider)}.
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    % ErlOpts = rebar_state:get(State, erl_opts, []),
    % NewState = rebar_state:set(State, erl_opts, [to_core | ErlOpts]),
    NewState = set_erl_opts(State),
    io:format("State:~n~p~n", [State]),
    io:format("NewState:~n~p~n", [NewState]),
    rebar_prv_compile:do(NewState).

-spec format_error(any()) -> iolist().
format_error(Reason) -> rebar_prv_compile:format_error(Reason).

%% ===================================================================
%% Private API
%% ===================================================================
set_erl_opts({state_t,
              Dir,
              Opts,
              CodePaths,
              Default,
              EscriptPath,
              Lock,
              CurrentProfiles,
              Namespace,
              CommandArgs,
              CommandParsedArgs,
              CurrentApp,
              ProjectApps,
              DepsToBuild,
              AllPluginDeps,
              AllDeps,
              Compilers,
              ProjectBuilders,
              Resources,
              Providers,
              AllowProviderOverrides}) ->
    NewOpts = dict:append(erl_opts, to_core, Opts),
    NewDefault = dict:append(erl_opts, to_core, Default),
    {state_t,
     Dir,
     NewOpts,
     CodePaths,
     NewDefault,
     EscriptPath,
     Lock,
     CurrentProfiles,
     Namespace,
     CommandArgs,
     CommandParsedArgs,
     CurrentApp,
     ProjectApps,
     DepsToBuild,
     AllPluginDeps,
     AllDeps,
     Compilers,
     ProjectBuilders,
     Resources,
     Providers,
     AllowProviderOverrides}.
