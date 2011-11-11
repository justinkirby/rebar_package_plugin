-module(rebar_package_plugin).

-export([package/2]).


-define(DEBUG(Msg, Args),
        rebar_log:log(debug, "[~p]  "++ Msg, [?MODULE|Args])).

-define(WARN(Msg, Args),
        rebar_log:log(warn, "[~p]  "++ Msg, [?MODULE|Args])).

-define(CONSOLE(Msg, Args),
        io:format(Msg,Args)).



package(Config, AppFile) ->
    ?CONSOLE("~p",[Config]).
