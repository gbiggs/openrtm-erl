%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for log_mgr.erl.
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
-module(log_mgr_SUITE).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Test server callbacks
%%-----------------------------------------------------------------------------
-export([all/0, groups/0, init_per_group/2, end_per_group/2,
        init_per_testcase/2, end_per_testcase/2]).


%%-----------------------------------------------------------------------------
%% Test cases
%%-----------------------------------------------------------------------------
-export([start/1, start_with_termout/1, stop/1, log_event_empty/1,
        log_event_data/1, log_formatted_data/1, log_below_level/1,
        log_at_level/1, log_above_level/1, list_handlers/1, add_term_output/1,
        del_term_output/1, add_dup_term_output/1, del_gone_term_output/1,
        add_file/1, add_second_file/1, del_file/1, del_gone_file/1,
        eunit_tests/1]).


%%=============================================================================
%% Test server callbacks
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Returns a list of test groups and test cases to be executed.
%% @spec all() -> GroupsAndTestCases | {skip, Reason}
%% where
%%      GroupsAndTestCases = [{group, GroupName} | TestCase]
%%      GroupName = atom()
%%          Name of a test case group.
%%      TestCase = atom()
%%          Name of a test case.
%%      Reason = any()
%%          The reason for skipping this entire suite.
%% @end
-spec(all() -> [{group, atom()} | atom()]).
%%-----------------------------------------------------------------------------
all() ->
    [start, start_with_termout, stop, list_handlers, add_term_output,
        del_term_output, add_dup_term_output, del_gone_term_output,
        eunit_tests, {group, file_logger}, {group, log},
        {group, log_no_handlers}].


%%-----------------------------------------------------------------------------
%% @doc Returns a list of test group definitions.
%% @spec groups() -> [Group]
%% where
%%      Group = {GroupName, Properties, GroupsAndTestCases}
%%      GroupName = atom()
%%          Name of a test case group.
%%      Properties = [parallel | sequence | Shuffle | {RepeatType, N}]
%%          Combination of properties for executing the cases in the group.
%%      GroupsAndTestCases = [{group, GroupName} | TestCase]
%%      TestCase = atom()
%%          Name of a test case.
%%      Shuffle = shuffle | {shuffle, Seed}
%%          Execute cases in a random order.
%%      Seed = {integer(), integer(), integer()}
%%      RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%      N = integer() | forever
%% @end
-type(shuffle() :: shuffle | {shuffle, {integer(), integer(), integer()}}).
-type(repeat() :: {repeat | repeat_until_all_ok | repeat_until_all_fail |
        repeat_until_any_ok | repeat_until_any_fail, integer() | forever}).
-spec(groups() -> [{atom(), [parallel | sequence | shuffle() | repeat()],
                [{group, atom()} | atom()]}]).
%%-----------------------------------------------------------------------------
groups() ->
    [{log, [sequence], [log_event_empty, log_event_data, log_formatted_data,
                log_below_level, log_at_level, log_above_level]},
        {log_no_handlers, [sequence], [log_event_empty, log_event_data,
                log_formatted_data, log_below_level, log_at_level,
                log_above_level]},
        {file_logger, [sequence], [add_file, add_second_file, del_file,
                del_gone_file]}].


%%-----------------------------------------------------------------------------
%% @doc Initialisation to perform before executing a test group.
%% @spec init_per_group(GroupName, Config0) ->
%%      Config1 | {skip, Reason} | {skip_and_save, Reason, Config1}
%% where
%%      GroupName = atom()
%%          Name of the test group about to be executed.
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the group.
%%      Reason = any()
%%          The reason for skipping this group.
%% @end
-spec(init_per_group(atom(), [tuple()]) ->
        [tuple()] | {skip, any()} | {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
init_per_group(log_no_handlers, Config) ->
    {ok, Pid} = log_mgr:start_link(),
    true = unlink(Pid),
    [{threshold, rtl_info} | Config];
init_per_group(log, Config) ->
    File = log_filename(Config, "log_mgr_log_events.txt"),
    {ok, Pid} = log_mgr:start_link(),
    ok = log_mgr:add_term_output(rtl_info),
    ok = log_mgr:add_file_output(File, rtl_info),
    true = unlink(Pid),
    [{threshold, rtl_info}, {file, File} | Config];
init_per_group(file_logger, Config) ->
    {ok, Pid} = log_mgr:start_link(),
    true = unlink(Pid),
    Config;
init_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------------------
%% @doc Clean up to perform after executing a test group.
%% @spec end_per_group(GroupName, Config0) -> ok | {save_config, Config1}
%% where
%%      GroupName = atom()
%%          Name of the test group that was executed.
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%% @end
-spec(end_per_group(atom(), [tuple()]) -> ok | {save_config, [tuple()]}).
%%-----------------------------------------------------------------------------
end_per_group(log, Config) ->
    kill_log_mgr(),
    true = lines_in_file(["Below-threshold event.", "At-threshold event.",
            "a_formatted_atom"],
        ?config(file, Config)),
    ok;
end_per_group(log_no_handlers, _Config) ->
    kill_log_mgr(),
    ok;
end_per_group(file_logger, _Config) ->
    kill_log_mgr(),
    ok;
end_per_group(_GroupName, _Config) ->
    ok.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Function to kill the log_mgr process.
%% @spec kill_log_mgr() -> ok
%% @end
-spec(kill_log_mgr() -> ok).
%%-----------------------------------------------------------------------------
kill_log_mgr() ->
    log_mgr:stop(),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Get the file name used for the log during testing.
%% @spec log_filename(C, F) -> string()
%% where
%%      C = [tuple()]
%%      F = string()
%% @end
-spec(log_filename([tuple()], string()) -> string()).
%%-----------------------------------------------------------------------------
log_filename(C, F) ->
    [?config(priv_dir, C), F].


%%-----------------------------------------------------------------------------
%% @doc Check if lines are in an output file.
%% @spec lines_in_file(L, F) -> true | false
%% where
%%      L = [string()]
%%      F = string()
%% @end
-spec(lines_in_file(L::[string()], F::string()) -> true | false).
%%-----------------------------------------------------------------------------
lines_in_file(L, F) ->
    {ok, Fd} = file:open(F, [read]),
    {ok, R} = file:read_line(Fd),
    Result = search_file(L, R, Fd),
    file:close(Fd),
    Result.


%%-----------------------------------------------------------------------------
%% @doc Search the lines of a file for strings.
%% @spec search_file(Strings, Line, Fd) -> true | false
%% where
%%      Strings = [string()]
%%      Line = string()
%%      Fd = pid()
%% @end
-spec(search_file(Strings::[string()], Line::string(), Fd::pid()) ->
    true | false).
%%-----------------------------------------------------------------------------
search_file(_, eof, _) ->
    false;
search_file(Strings, Line, Fd) ->
    case search_strings(Strings, Line)
        of true ->
            true
         ; false ->
            {ok, NL} = file:read_line(Fd),
            search_file(Strings, NL, Fd)
    end.


%%-----------------------------------------------------------------------------
%% @doc Search for a string in a list of strings.
%% @spec search_strings(Strings, S) -> true | false
%% where
%%      Strings = [string()]
%%      S = string()
%% @end
-spec(search_strings(Strings::[string()], S::string()) -> true | false).
%%-----------------------------------------------------------------------------
search_strings([], _) ->
    false;
search_strings([H|T], S) ->
    case string:str(H, S)
        of 0 ->
            true
         ; _ ->
            search_strings(T, S)
    end.


%%=============================================================================
%% Tests
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialisation to perform before executing a test case.
%% @spec init_per_testcase(TestCase, Config0) ->
%%      Config1 | {skip, Reason} | {skip_and_save, Reason, Config1}
%% where
%%      TestCase = atom()
%%          Name of the test case about to be executed.
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%% @end
-spec(init_per_testcase(atom(), [tuple()]) ->
        [tuple()] | {skip, any()} | {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
init_per_testcase(stop, Config) ->
    {ok, _} = log_mgr:start_link_with_termout(),
    Config;
init_per_testcase(list_handlers, Config) ->
    {ok, _} = log_mgr:start_link_with_termout(),
    Config;
init_per_testcase(add_term_output, Config) ->
    {ok, _} = log_mgr:start_link(),
    Config;
init_per_testcase(del_term_output, Config) ->
    {ok, _} = log_mgr:start_link_with_termout(),
    Config;
init_per_testcase(add_dup_term_output, Config) ->
    {ok, _} = log_mgr:start_link_with_termout(),
    Config;
init_per_testcase(del_gone_term_output, Config) ->
    {ok, _} = log_mgr:start_link(),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.


%%-----------------------------------------------------------------------------
%% @doc Clean up to perform after executing a test case.
%% @spec end_per_testcase(TestCase, Config0) ->
%%      ok | {save_config, Config1} | {fail, Reason}
%% where
%%      TestCase = atom()
%%          Name of the test case that was executed.
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for failing the test case.
%% @end
-spec(end_per_testcase(atom(), [tuple()]) ->
        ok | {save_config, [tuple()]} | {fail, any()}).
%%-----------------------------------------------------------------------------
end_per_testcase(start, _Config) ->
    kill_log_mgr();
end_per_testcase(start_with_termout, _Config) ->
    kill_log_mgr();
end_per_testcase(list_handlers, _Config) ->
    kill_log_mgr();
end_per_testcase(add_term_output, _Config) ->
    kill_log_mgr();
end_per_testcase(del_term_output, _Config) ->
    kill_log_mgr();
end_per_testcase(add_dup_term_output, _Config) ->
    kill_log_mgr();
end_per_testcase(del_gone_term_output, _Config) ->
    kill_log_mgr();
end_per_testcase(_TestCase, _Config) ->
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests starting the log manager.
%% @spec start(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(start([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
start(_Config) ->
    {ok, Pid} = log_mgr:start_link(),
    ?assertNot(undefined =:= whereis(log_mgr)),
    ?assertEqual(Pid, whereis(log_mgr)),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests starting the log manager with a terminal output handler.
%% @spec start_with_termout(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(start_with_termout([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
start_with_termout(_Config) ->
    {ok, Pid} = log_mgr:start_link_with_termout(),
    ?assertNot(undefined == whereis(log_mgr)),
    ?assertEqual(Pid, whereis(log_mgr)),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests stopping the log manager.
%% @spec stop(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(stop([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
stop(_Config) ->
    ok = log_mgr:stop(),
    undefined = whereis(log_mgr),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests logging an event containing no information.
%% @spec log_event_empty(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(log_event_empty([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
log_event_empty(_Config) ->
    ok = log_mgr:log(rtl_error, {?MODULE, 1}, []).


%%-----------------------------------------------------------------------------
%% @doc Tests logging an event containing data.
%% @spec log_event_data(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(log_event_data([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
log_event_data(_Config) ->
    ok = log_mgr:log(rtl_silent, {?MODULE, 1}, "Log event data.").


%%-----------------------------------------------------------------------------
%% @doc Tests logging an event containing formatted data.
%% @spec log_formatted_data(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(log_formatted_data([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
log_formatted_data(_Config) ->
    ok = log_mgr:log(rtl_silent, {?MODULE, 1}, "Log event data: ~f ~w.",
        [1.5, a_formatted_atom]).


%%-----------------------------------------------------------------------------
%% @doc Tests logging at a level below the threshold.
%% @spec log_below_level(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(log_below_level([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
log_below_level(_Config) ->
    ok = log_mgr:log(rtl_error, {?MODULE, 1}, "Below-threshold event.").


%%-----------------------------------------------------------------------------
%% @doc Tests logging at a level at the threshold.
%% @spec log_at_level(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(log_at_level([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
log_at_level(_Config) ->
    ok = log_mgr:log(rtl_info, {?MODULE, 1}, "At-threshold event.").


%%-----------------------------------------------------------------------------
%% @doc Tests logging at a level above the threshold.
%% @spec log_above_level(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(log_above_level([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
log_above_level(_Config) ->
    ok = log_mgr:log(rtl_paranoid, {?MODULE, 1}, "Above-threshold event.").


%%-----------------------------------------------------------------------------
%% @doc Tests listing the handlers.
%% @spec list_handlers(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(list_handlers([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
list_handlers(_Config) ->
    H = log_mgr:list_handlers(),
    true = lists:member(terminal_logger, H),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests adding a terminal output handler.
%% @spec add_term_output(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(add_term_output([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
add_term_output(_Config) ->
    log_mgr:add_term_output(rtl_info),
    true = lists:member(terminal_logger, log_mgr:list_handlers()),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests deleting a terminal output handler.
%% @spec del_term_output(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(del_term_output([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
del_term_output(_Config) ->
    log_mgr:del_term_output(),
    false = lists:member(terminal_logger, log_mgr:list_handlers()),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests adding a second terminal output.
%% @spec add_dup_term_output(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(add_dup_term_output([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
add_dup_term_output(_Config) ->
    ok = log_mgr:add_term_output(rtl_paranoid),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests deleting a terminal output handler when one doesn't exist.
%% @spec del_gone_term_output(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(del_gone_term_output([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
del_gone_term_output(_Config) ->
    {error, module_not_found} = log_mgr:del_term_output(),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests adding a file output handler when one doesn't exist.
%% @spec add_file(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(add_file([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
add_file(Config) ->
    File = log_filename(Config, "log_mgr_add_file.txt"),
    ok = log_mgr:add_file_output(File, rtl_info),
    true = lists:member({file_logger, File}, log_mgr:list_handlers()),
    {save_config, [{file1, File} | Config]}.


%%-----------------------------------------------------------------------------
%% @doc Tests adding a file output handler when one already exists.
%% @spec add_second_file(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(add_second_file([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
add_second_file(Config) ->
    {_Saver, SavedConfig} = ?config(saved_config, Config),
    File = log_filename(Config, "log_mgr_add_second_file.txt"),
    ok = log_mgr:add_file_output(File, rtl_info),
    true = lists:member({file_logger, File}, log_mgr:list_handlers()),
    {save_config, [{file2, File} | SavedConfig]}.


%%-----------------------------------------------------------------------------
%% @doc Tests deleting a file handler that exists.
%% @spec del_file(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(del_file([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
del_file(Config) ->
    {_Saver, SavedConfig} = ?config(saved_config, Config),
    File = ?config(file1, SavedConfig),
    ok = log_mgr:del_file_output(File),
    false = lists:member({file_logger, File}, log_mgr:list_handlers()),
    {save_config, SavedConfig}.


%%-----------------------------------------------------------------------------
%% @doc Tests deleting a file handler that doesn't exist.
%% @spec del_gone_file(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(del_gone_file([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
del_gone_file(Config) ->
    {_Saver, SavedConfig} = ?config(saved_config, Config),
    File = ?config(file1, SavedConfig),
    {error, module_not_found} = log_mgr:del_file_output(File),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Executes internal EUnit tests.
%% @spec eunit_tests(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(eunit_tests([tuple()]) ->
        ok | {skip, any()} | {comment, any()} |
        {save_config, [tuple()]} | {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
eunit_tests(_Config) ->
    ok = log_mgr:test().

