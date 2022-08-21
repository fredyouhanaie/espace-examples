-module(tsudoku).

%% API exports
-export([main/1]).

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------

-define(Log_level, notice).

-define(Version, "0.1.0").

-define(Opt_specs,
        [
         %%{Name,   ShortOpt,  LongOpt,    ArgSpec,        HelpMsg}
         {help,     $h,        "help",     undefined,      "Print help."},
         {version,  $v,        "version",  undefined,      "Print version."},
         {loglevel, $l,        "loglevel", {atom, notice}, "Set log level."}
        ]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->

    logger:set_primary_config(level, ?Log_level),

    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),

    ?LOG_DEBUG(#{func => ?FUNCTION_NAME, msg => "startup", args => Args} ),
    case getopt:parse(?Opt_specs, Args) of
        {error, {Reason, Data}} ->
            ?LOG_DEBUG(#{func => ?FUNCTION_NAME,
                         reason => Reason,
                         data => Data}),
            getopt:usage(?Opt_specs, "mcscripts");

        {ok, {Parsed, Rest}} ->
            ?LOG_DEBUG(#{func => ?FUNCTION_NAME,
                         parsed => Parsed,
                         rest => Rest}),
            process_args(Parsed, Rest)

    end,

    timer:sleep(0),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-define(Process_opt(Opt, Action),
        case (proplists:get_value(Opt, Opts)) of
            true -> Action;
            _    -> ok
        end).

process_args(Opts, Args) ->
    logger:set_primary_config(level, proplists:get_value(loglevel, Opts)),

    ?Process_opt(version, io:format("Version ~p.~n", [?Version])),
    ?Process_opt(help,    getopt:usage(?Opt_specs, atom_to_list(?MODULE))),

    ok.

%%--------------------------------------------------------------------
