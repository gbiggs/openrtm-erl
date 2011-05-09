%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Log manager responsible for launching and managing logging facilities.
%% @reference <a href="http://www.openrtm.org/">OpenRTM.org</a>
%% @copyright 2010 Geoffrey Biggs
%% RT-Synthesis Research Group
%% Intelligent Systems Research Institute,
%% National Institute of Advanced Industrial Science and Technology (AIST),
%% Japan
%% All rights reserved.
%% Licensed under the Eclipse Public License -v 1.0 (EPL)
%% http://www.opensource.org/licenses/eclipse-1.0.txt
%% @version {@version}
%% @end
%%-----------------------------------------------------------------------------
-module(log_mgr).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------

% Server management
-export([start_link/0, start_link_with_outputs/0, start_link_with_termout/0,
        stop/0]).
% Output management
-export([add_term_output/1, del_term_output/0, add_file_output/2,
        del_file_output/1, list_handlers/0]).
% Logging
-export([log/3, log/4]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Activate the event handler that publishes to the terminal. Does nothing
%% if the handler is already active.
%% @spec add_term_output(Level) -> ok | {error, Reason}
%% where
%%      Level = log_level()
%%      Reason = any()
%% @end
-spec(add_term_output(Level::log_level()) -> ok | {error, any()}).
%%-----------------------------------------------------------------------------
add_term_output(Level) ->
    case lists:member(terminal_logger, list_handlers())
        of true ->
            ok
         ; _ ->
            add_handler(terminal_logger, Level)
    end.


%%-----------------------------------------------------------------------------
%% @doc Deactivate the event handler that publishes to the terminal.
%% @spec del_term_output() -> ok | {error, Reason}
%% where
%%      Reason = any()
%% @end
-spec(del_term_output() -> ok | {error, any()}).
%%-----------------------------------------------------------------------------
del_term_output() ->
    case gen_event:delete_handler(log_mgr, terminal_logger, [])
        of ok ->
            ok
         ; {error, Reason} ->
            {error, Reason}
         ; Reason ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Activate an event handler that publishes to a file. Duplicate file
%% publishers are possible; they are identified by their file name.
%% @spec add_file_output(File, Level) -> ok | {error, Reason}
%% where
%%      File = string()
%%      Level = log_level()
%%      Reason = any()
%% @end
-spec(add_file_output(File::string(), Level::log_level()) ->
        ok | {error, any()}).
%%-----------------------------------------------------------------------------
add_file_output(File, Level) ->
    add_handler({file_logger, File}, {File, Level}).


%%-----------------------------------------------------------------------------
%% @doc Deactivate an event handler that publishes to a file. The file name
%% must be provided to identify the handler.
%% @spec del_file_output(File) -> ok | {error, Reason}
%% where
%%      File = string()
%%      Reason = any()
%% @end
-spec(del_file_output(File::string()) -> ok | {error, any()}).
%%-----------------------------------------------------------------------------
del_file_output(File) ->
    case gen_event:delete_handler(log_mgr, {file_logger, File}, [])
        of ok ->
            ok
         ; {error, Reason} ->
            {error, Reason}
         ; Reason ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Get the list of active event handlers.
%% @spec list_handlers() -> [HandlerRef]
%% where
%%      HandlerRef = [atom() | {atom(), any()}]
%% @end
-spec(list_handlers() -> [atom() | {atom(), any()}]).
%%-----------------------------------------------------------------------------
list_handlers() ->
    gen_event:which_handlers(log_mgr).


%%-----------------------------------------------------------------------------
%% @doc Log an event to the currently-active event handlers. The event is a
%% tuple of the priority level, the module/line in which the event occured, and
%% the event data. If the log manager is not active, the event will be dumped
%% to standard error.
%% @spec log(Level, {Module, Line, Pid}, String) -> ok
%% where
%%      Level = log_level()
%%      Module = atom()
%%      Line = pos_integer()
%%      Pid = pid()
%%      String = string()
%% @end
-spec(log(Level::log_level(),
        {Module::atom(), Line::pos_integer(), Pid::pid()}, String::string()) ->
    ok).
%%-----------------------------------------------------------------------------
log(Level, ModLine, String) ->
    case whereis(log_mgr)
        of undefined ->
            log_without_logger(Level, ModLine, String)
         ; _ ->
            gen_event:notify(log_mgr, {Level, ModLine, String})
    end.

%%-----------------------------------------------------------------------------
%% @doc Log an event to the currently-active event handlers. The event is a
%% tuple of the priority level, the module/line in which the event occured, a
%% format string and data to be formatted. If the log manager is not active,
%% the event will be dumped to standard error.
%% @spec log(Level, {Module, Line, Pid}, String, Data) -> ok
%% where
%%      Level = log_level()
%%      Module = atom()
%%      Line = pos_integer()
%%      Pid = pid()
%%      String = string()
%%      Data = [any()]
%% @end
-spec(log(Level::log_level(),
        {Module::atom(), Line::pos_integer(), Pid::pid()},
        String::string(), Data::any()) ->
    ok).
%%-----------------------------------------------------------------------------
log(Level, ModLine, String, Data) ->
    S = io_lib:format(String, Data),
    log(Level, ModLine, S).


%%-----------------------------------------------------------------------------
%% @doc Start the log manager.
%% @spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}
%% @end
-spec(start_link() -> {ok, pid()} | {error, {already_started, pid()}}).
%%-----------------------------------------------------------------------------
start_link() ->
    reg_levels(),
    gen_event:start_link({local, log_mgr}).


%%-----------------------------------------------------------------------------
%% @doc Start the log manager, query the config server for the active outputs,
%% and start them.
%% @spec start_link_with_outputs() ->
%%      {ok, pid()} | {error, {already_started, pid()}}
%% @end
-spec(start_link_with_outputs() ->
    {ok, pid()} | {error, {already_started, pid()}}).
%%-----------------------------------------------------------------------------
start_link_with_outputs() ->
    reg_levels(),
    {ok, Pid} = gen_event:start_link({local, log_mgr}),
    ok = add_active_outputs(),
    {ok, Pid}.


%%-----------------------------------------------------------------------------
%% @doc Start the log manager, and add a terminal output.
%% @spec start_link_with_termout() ->
%%      {ok, pid()} | {error, {already_started, pid()}}
%% @end
-spec(start_link_with_termout() ->
    {ok, pid()} | {error, {already_started, pid()}}).
%%-----------------------------------------------------------------------------
start_link_with_termout() ->
    reg_levels(),
    {ok, Pid} = gen_event:start_link({local, log_mgr}),
    ok = add_term_output(rtl_info),
    {ok, Pid}.


%%-----------------------------------------------------------------------------
%% @doc Stop the log manager, stopping all event handlers.
%% @spec stop() -> ok
%% @end
-spec(stop() -> ok).
%%-----------------------------------------------------------------------------
stop() ->
    ?LOG(rtl_info, "Logging system shutting down normally."),
    gen_event:stop(log_mgr).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Add a new event handler.
%% @spec add_handler(Module, Args) -> ok | {error, Reason}
%% where
%%      Module = atom() | {atom(), any()}
%%      Args = any()
%%      Reason = any()
%% @end
-spec(add_handler(atom() | {atom(), any()}, any()) -> ok | {error, any()}).
%%-----------------------------------------------------------------------------
add_handler(Module, Args) ->
    case gen_event:add_handler(log_mgr, Module, Args)
        of ok ->
            ok
         ; {'EXIT', Reason} ->
            {error, Reason}
         ; Reason ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Query the configuration server for the active outputs and add them.
%% @spec add_active_outputs() -> ok | {error, Reason}
%% where
%%      Reason = any()
%% @end
-spec(add_active_outputs() -> ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
add_active_outputs() ->
    case config_svr:get_value(["logger", "enable"])
        of true ->
            lists:foreach(fun add_output_by_str/1,
                config:tokens(config_svr:get_value(["logger",
                            "outputs"]), ","))
         ; false ->
            ok
    end.


%%-----------------------------------------------------------------------------
%% @doc Add an output by string representation.
%% @spec add_output_by_str(string()) -> ok | {error, Reason}
%% where
%%      Reason = any()
%% @end
-spec(add_output_by_str(Output::string()) -> ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
add_output_by_str("terminal") ->
    {Type, Level} = try
        list_to_existing_atom(config_svr:get_value(["logger", "terminal",
                    "level"])) of
        L ->
            {ok, L}
    catch
        error:badarg ->
            {error, rtl_warn}
    end,
    add_term_output(Level),
    case Type
        of error ->
            ?LOG(rtl_warn, "Bad log level for terminal logger: ~p",
                [config_svr:get_value(["logger", "terminal", "level"])])
         ; _ ->
            ok
    end;
add_output_by_str("file") ->
    File = process_fn(config_svr:get_value(["logger", "file", "name"])),
    {Type, Level} = try
        list_to_existing_atom(config_svr:get_value(["logger", "file",
                    "level"])) of
        L ->
            {ok, L}
    catch
        error:badarg ->
            {error, rtl_info}
    end,
    add_file_output(File, Level),
    case Type
        of error ->
            ?LOG(rtl_warn, "Bad log level for file logger: ~p",
                [config_svr:get_value(["logger", "terminal", "level"])])
         ; _ ->
            ok
    end;
add_output_by_str(Type) ->
    ?LOG(rtl_error, "Unknown output type in configuration file: ~p", [Type]),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Dump an event to standard_error when no log manager is running, but
%% only when LOG_WIHTOUT_LOGGER is enabled at compile time.
%% @spec log_without_logger(Level, {Module, Line, Pid}, String) -> ok
%% where
%%      Level = log_level()
%%      Module = atom()
%%      Line = pos_integer()
%%      Pid = pid()
%%      String = string()
%% @end
-spec(log_without_logger(Level::log_level(),
        {Module::atom(), Line::pos_integer(), Pid::pid()}, String::string()) ->
    ok).
%%-----------------------------------------------------------------------------
-ifdef(LOG_WITHOUT_LOGGER).
log_without_logger(Level, ModLine, String) ->
    io:format(standard_error, "L/NoLM: ~s~n",
        [event_fmt:format(Level, ModLine, String)]).
-else. % LOG_WITHOUT_LOGGER
log_without_logger(_Level, _ModLine, _String) ->
    ok.
-endif. % LOG_WITHOUT_LOGGER


%%-----------------------------------------------------------------------------
%% @doc Process a file name to replace wildcards.
%% @spec process_fn(string()) -> string()
%% @end
-spec(process_fn(FN::string()) -> string()).
%%-----------------------------------------------------------------------------
process_fn(FN) ->
    re:replace(FN, "%p", atom_to_list(node()), [{return, list}]).


%%-----------------------------------------------------------------------------
%% @doc Ensures the log level atoms exist for use with list_to_existing_atom/1.
%% @spec reg_levels() -> ok
%% @end
-spec(reg_levels() -> ok).
%%-----------------------------------------------------------------------------
reg_levels() ->
    rtl_silent,
    rtl_fatal,
    rtl_error,
    rtl_warn,
    rtl_info,
    rtl_debug,
    rtl_trace,
    rtl_verbose,
    rtl_paranoid,
    ok.


%%=============================================================================
%% Test functions
%%=============================================================================

-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test adding outputs by string as if from a config file.
%% @spec add_output_by_str_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(add_output_by_str_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
add_output_by_str_test_() ->
    {setup,
        fun() ->
            start_link(),
            start_mock_config_svr(defaults:defaults())
        end,
        fun(_) ->
            stop(),
            stop_mock_config_svr(),
            file:delete("rtcnonode@nohost.log")
        end,
        fun(_) ->
            [?_assertMatch(ok, add_output_by_str("terminal")),
                ?_assertMatch(true, lists:member(terminal_logger,
                        list_handlers())),
                ?_assertMatch(ok, add_output_by_str("file")),
                ?_assertMatch(true, lists:keymember(file_logger, 1,
                        list_handlers())),
                ?_assertMatch(ok, add_output_by_str("disc"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test adding outputs by string as if from a config file, but with bad
%% values.
%% @spec add_output_by_str_bad_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(add_output_by_str_bad_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
add_output_by_str_bad_test_() ->
    {setup,
        fun() ->
            start_link(),
            start_mock_config_svr([{"logger", [{"enable", true},
                            {"file", [{"name", "rtc%p.log"},
                                    {"level", "rtl_notalevel"}]},
                            {"terminal", [{"level", "rtl_notalevelatall"}]},
                            {"outputs", "terminal, file"}]}])
        end,
        fun(_) ->
            stop(),
            stop_mock_config_svr(),
            file:delete("rtcnonode@nohost.log")
        end,
        fun(_) ->
            [?_assertMatch(ok, add_output_by_str("terminal")),
                ?_assertMatch(true, lists:member(terminal_logger,
                        list_handlers())),
                ?_assertMatch(ok, add_output_by_str("file")),
                ?_assertMatch(true, lists:keymember(file_logger, 1,
                        list_handlers()))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test adding the configured default handlers.
%% @spec add_active_outputs_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(add_active_outputs_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
add_active_outputs_test_() ->
    {setup,
        fun() ->
            start_link(),
            start_mock_config_svr(defaults:defaults())
        end,
        fun(_) ->
            stop(),
            stop_mock_config_svr(),
            file:delete("rtcnonode@nohost.log")
        end,
        fun(_) ->
                [?_assertMatch(ok, add_active_outputs()),
                ?_assertMatch(true, lists:member(terminal_logger,
                        list_handlers())),
                ?_assertMatch(true, lists:keymember(file_logger, 1,
                        list_handlers()))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test adding the configured default handlers with only term configured.
%% @spec add_active_outputs_just_term_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(add_active_outputs_just_term_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
add_active_outputs_just_term_test_() ->
    {setup,
        fun() ->
            start_link(),
            start_mock_config_svr(config:merge(defaults:defaults(),
                    [{"logger", [{"outputs", "terminal"}]}]))
        end,
        fun(_) ->
            stop(),
            stop_mock_config_svr(),
            file:delete("rtcnonode@nohost.log")
        end,
        fun(_) ->
                [?_assertMatch(ok, add_active_outputs()),
                ?_assertMatch(true, lists:member(terminal_logger,
                        list_handlers())),
                ?_assertMatch(false, lists:keymember(file_logger, 1,
                        list_handlers()))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test adding the configured default handlers with only file configured.
%% @spec add_active_outputs_just_file_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(add_active_outputs_just_file_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
add_active_outputs_just_file_test_() ->
    {setup,
        fun() ->
            start_link(),
            start_mock_config_svr(config:merge(defaults:defaults(),
                    [{"logger", [{"outputs", "file"}]}]))
        end,
        fun(_) ->
            stop(),
            stop_mock_config_svr(),
            file:delete("rtcnonode@nohost.log")
        end,
        fun(_) ->
                [?_assertMatch(ok, add_active_outputs()),
                ?_assertMatch(false, lists:member(terminal_logger,
                        list_handlers())),
                ?_assertMatch(true, lists:keymember(file_logger, 1,
                        list_handlers()))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test adding the configured default handlers with only a bad one
%% configured.
%% @spec add_active_outputs_just_bad_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(add_active_outputs_just_bad_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
add_active_outputs_just_bad_test_() ->
    {setup,
        fun() ->
            start_link(),
            start_mock_config_svr(config:merge(defaults:defaults(),
                    [{"logger", [{"outputs", "disc"}]}]))
        end,
        fun(_) ->
            stop(),
            stop_mock_config_svr(),
            file:delete("rtcnonode@nohost.log")
        end,
        fun(_) ->
                [?_assertMatch(ok, add_active_outputs()),
                ?_assertMatch(false, lists:member(terminal_logger,
                        list_handlers())),
                ?_assertMatch(false, lists:keymember(file_logger, 1,
                        list_handlers()))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test adding the configured default handlers with a bad one configured.
%% @spec add_active_outputs_with_bad_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(add_active_outputs_with_bad_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
add_active_outputs_with_bad_test_() ->
    {setup,
        fun() ->
            start_link(),
            start_mock_config_svr(config:merge(defaults:defaults(),
                    [{"logger", [{"outputs", "terminal, disc"}]}]))
        end,
        fun(_) ->
            stop(),
            stop_mock_config_svr(),
            file:delete("rtcnonode@nohost.log")
        end,
        fun(_) ->
                [?_assertMatch(ok, add_active_outputs()),
                ?_assertMatch(true, lists:member(terminal_logger,
                        list_handlers())),
                ?_assertMatch(false, lists:keymember(file_logger, 1,
                        list_handlers()))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test file name processing
%% @spec process_fn_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(process_fn_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
process_fn_test_() ->
    [?_assertMatch("rtc.log", process_fn("rtc.log")),
        ?_assertEqual("rtc" ++ atom_to_list(node()) ++ ".log",
            process_fn("rtc%p.log"))].


%%-----------------------------------------------------------------------------
%% @doc Start a configuration server to provide data during the unit tests.
%% @spec start_mock_config_svr(Defaults) -> {ok, pid()}
%% where
%%      Defaults = config()
%% @end
-spec(start_mock_config_svr(Defaults::config()) -> {ok, pid()}).
%%-----------------------------------------------------------------------------
start_mock_config_svr(Defaults) ->
    {ok, _} = config_svr:start_link("", Defaults).


%%-----------------------------------------------------------------------------
%% @doc Stop the configuration server.
%% @spec stop_mock_config_svr() -> ok
%% @end
-spec(stop_mock_config_svr() -> ok).
%%-----------------------------------------------------------------------------
stop_mock_config_svr() ->
    config_svr:stop(),
    timer:sleep(100).


-endif. % TEST

