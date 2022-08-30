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
            usage();

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
            true -> Action, true;
            _    -> false
        end).

-spec process_args(proplists:proplist(), list()) -> ok|error.
process_args(Opts, Args) ->
    logger:set_primary_config(level, proplists:get_value(loglevel, Opts)),

    ?Process_opt(version, io:format("Version ~p.~n", [?Version]))
        orelse ?Process_opt(help,    usage())
        orelse process_command(Args).

%%--------------------------------------------------------------------

usage() ->
    io:format("Version ~p.~n", [?Version]),
    getopt:usage(?Opt_specs, atom_to_list(?MODULE), "command ...",
                 [ {"command", "command to execute, e.g. solve, check, ..."} ]).

%%--------------------------------------------------------------------

-spec process_command(list()) -> ok|error.
process_command([]) ->
    ok;

process_command([Cmd|Args]) ->
    do_command(list_to_atom(Cmd), Args).

%%--------------------------------------------------------------------

-spec do_command(atom(), list()) -> ok|error.
do_command(solve, Args) ->
    do_solve(Args);

do_command(check, Args) ->
    do_check(Args);

do_command(C, A) ->
    ?LOG_ERROR("command ~p is undefined, args=~p.~n", [C, A]),
    error.

%%--------------------------------------------------------------------

do_solve([File]) ->
    case tsudoku1:solve_file(File) of
        ok ->
            {[{ok, Solution}], _} = espace:rd({done, '$1'}),
            io:format("~p~n", [Solution]),
            ok;
        _ ->
            ?LOG_ERROR("solve: There was a problem"),
            error
    end;

do_solve(_) ->
    ?LOG_ERROR("solve: A single filename is expected"),
    error.


%%--------------------------------------------------------------------

do_check(_Args) ->
    ok.

%%--------------------------------------------------------------------
