-module(rebar3_to_core_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, to_core).
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
                          {example, "rebar3 to_core"}, % How to use the plugin
                          {opts, []},                   % list of options understood by the plugin
                          {short_desc, "A rebar plugin"},
                          {desc, "A rebar plugin"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    UpdateErlOpts =
        fun(AppInfo) ->
           ErlOpts = rebar_app_info:get(AppInfo, erl_opts, []),
           rebar_app_info:set(AppInfo, erl_opts, [to_core | ErlOpts])
        end,
    NewProjectApps = lists:map(UpdateErlOpts, rebar_state:project_apps(State)),
    F = fun(App, State) -> rebar_state:project_apps(State, App) end,
    NewState = lists:foldl(F, State, NewProjectApps),
    rebar_prv_compile:do(NewState).

-spec format_error(any()) -> iolist().
format_error(Reason) -> rebar_prv_compile:format_error(Reason).
