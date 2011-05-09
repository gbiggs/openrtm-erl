%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for buffer_svr.erl.
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
-module(buffer_svr_SUITE).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Test server callbacks
%%-----------------------------------------------------------------------------
-export([all/0, init_per_testcase/2, end_per_testcase/2]).


%%-----------------------------------------------------------------------------
%% Test cases
%%-----------------------------------------------------------------------------
-export([eunit_tests/1, start_stop/1, wrc/1]).


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
    [eunit_tests, start_stop, wrc].


%%=============================================================================
%% Internal functions
%%=============================================================================

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
init_per_testcase(wrc, Config) ->
    {ok, Svr} = buffer_svr:start_link(queue_buffer, [{"size", 3}]),
    [{svr, Svr} | Config];
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
end_per_testcase(wrc, Config) ->
    Svr = ?config(svr, Config),
    ok = buffer_svr:stop_server(Svr);
end_per_testcase(_TestCase, _Config) ->
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
    ok = buffer_svr:test().


%%-----------------------------------------------------------------------------
%% @doc Tests starting and stopping the server.
%% @spec start_stop(Config0) ->
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
-spec(start_stop([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
start_stop(_Config) ->
    {ok, Svr} = buffer_svr:start_link(queue_buffer, []),
    true = is_process_alive(Svr),
    ok = buffer_svr:stop_server(Svr),
    timer:sleep(100),
    false = is_process_alive(Svr).


%%-----------------------------------------------------------------------------
%% @doc Tests writing to, reading from and counting the items in the buffer.
%% @spec wrc(Config0) ->
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
-spec(wrc([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
wrc(Config) ->
    Svr = ?config(svr, Config),
    {error, empty} = buffer_svr:read(Svr),
    ok = buffer_svr:write(Svr, 1),
    1 = buffer_svr:count(Svr),
    1 = buffer_svr:read(Svr),
    ok = buffer_svr:write(Svr, 1),
    1 = buffer_svr:count(Svr),
    ok = buffer_svr:write(Svr, 2),
    2 = buffer_svr:count(Svr),
    ok = buffer_svr:write(Svr, 3),
    3 = buffer_svr:count(Svr),
    overflow = buffer_svr:write(Svr, 5),
    3 = buffer_svr:count(Svr),
    2 = buffer_svr:read(Svr),
    2 = buffer_svr:count(Svr),
    3 = buffer_svr:read(Svr),
    1 = buffer_svr:count(Svr),
    5 = buffer_svr:read(Svr),
    0 = buffer_svr:count(Svr),
    ok = buffer_svr:write(Svr, 8),
    ok = buffer_svr:write(Svr, 13),
    ok = buffer_svr:write(Svr, 21),
    3 = buffer_svr:count(Svr),
    overflow = buffer_svr:write(Svr, 34),
    13 = buffer_svr:read(Svr),
    2 = buffer_svr:count(Svr),
    ok = buffer_svr:write(Svr, 55),
    3 = buffer_svr:count(Svr).

