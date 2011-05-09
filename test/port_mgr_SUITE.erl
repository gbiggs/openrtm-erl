%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for port_mgr.erl.
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
-module(port_mgr_SUITE).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("port.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Test server callbacks
%%-----------------------------------------------------------------------------
-export([all/0, init_per_suite/1, end_per_suite/1, groups/0, init_per_group/2,
        end_per_group/2, init_per_testcase/2, end_per_testcase/2]).


%%-----------------------------------------------------------------------------
%% Test cases
%%-----------------------------------------------------------------------------
-export([eunit_tests/1, start_stop/1, add_datain_port/1, add_dataout_port/1,
        add_svc_port/1, rem_datain_port/1, rem_dataout_port/1,
        rem_svc_port/1, destroy_ports/1, get_ports/1, port_profs/1]).


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
    [eunit_tests, start_stop, {group, add_port}, {group, rem_port},
        {group, port_intro}, destroy_ports].


%%-----------------------------------------------------------------------------
%% @doc Initialisation to perform before executing the test suite.
%% @spec init_per_suite(Config0) -> Config1
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the suite.
%% @end
-spec(init_per_suite([tuple()]) -> [tuple()]).
%%-----------------------------------------------------------------------------
init_per_suite(Config) ->
    case whereis(orber_sup)
        of undefined ->
            orber:jump_start([{iiop_port,28090}])
         ; _ ->
            ok
    end,
    application:load(sasl),
    application:start(sasl),
    Config.


%%-----------------------------------------------------------------------------
%% @doc Clean up to perform after executing the test suite.
%% @spec end_per_suite(Config) -> ok
%% where
%%      Config = [tuple()]
%%          A list of key/value pairs configuring the suite.
%% @end
-spec(end_per_suite([tuple()]) -> ok).
%%-----------------------------------------------------------------------------
end_per_suite(_Config) ->
    orber:stop(),
    orber:uninstall(),
    mnesia:stop(),
    ok.


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
    [{add_port, [shuffle], [add_datain_port, add_dataout_port, add_svc_port]},
        {rem_port, [shuffle],
            [rem_datain_port, rem_dataout_port, rem_svc_port]},
        {port_intro, [shuffle], [get_ports, port_profs]}].


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
init_per_group(add_port, Config) ->
    {ok, PMPid} = port_mgr:start_link(),
    true = unlink(PMPid),
    {ok, PSPid} = ports_sup:start_link(),
    true = unlink(PSPid),
    ok = port_mgr:set_ports_sup(PMPid, PSPid),
    [{pm, PMPid}, {ps, PSPid} | Config];
init_per_group(rem_port, Config) ->
    {ok, PMPid} = port_mgr:start_link(),
    true = unlink(PMPid),
    {ok, PSPid} = ports_sup:start_link(),
    true = unlink(PSPid),
    ok = port_mgr:set_ports_sup(PMPid, PSPid),
    ok = port_mgr:add_port(PMPid, datain, "DataIn", "TimedLong", self(), []),
    ok = port_mgr:add_port(PMPid, dataout, "DataOut", "TimedLong", self(), []),
    %ok = port_mgr:add_port(PMPid, svc, "Svc", "", []),
    [{pm, PMPid}, {ps, PSPid} | Config];
init_per_group(port_intro, Config) ->
    {ok, PMPid} = port_mgr:start_link(),
    true = unlink(PMPid),
    {ok, PSPid} = ports_sup:start_link(),
    true = unlink(PSPid),
    ok = port_mgr:set_ports_sup(PMPid, PSPid),
    ok = port_mgr:add_port(PMPid, datain, "DataIn", "TimedLong", self(), []),
    ok = port_mgr:add_port(PMPid, dataout, "DataOut", "TimedLong", self(), []),
    %ok = port_mgr:add_port(PMPid, svc, "Svc", "", []),
    [{pm, PMPid}, {ps, PSPid}, {owner, self()} | Config];
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
end_per_group(add_port, Config) ->
    Pid = ?config(pm, Config),
    port_mgr:stop_server(Pid);
end_per_group(rem_port, Config) ->
    Pid = ?config(pm, Config),
    port_mgr:stop_server(Pid);
end_per_group(port_intro, Config) ->
    Pid = ?config(pm, Config),
    port_mgr:stop_server(Pid);
end_per_group(_GroupName, _Config) ->
    ok.


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
init_per_testcase(destroy_ports, Config) ->
    {ok, PMPid} = port_mgr:start_link(),
    true = unlink(PMPid),
    {ok, PSPid} = ports_sup:start_link(),
    true = unlink(PSPid),
    ok = port_mgr:set_ports_sup(PMPid, PSPid),
    ok = port_mgr:add_port(PMPid, datain, "DataIn", "TimedLong", self(), []),
    ok = port_mgr:add_port(PMPid, dataout, "DataOut", "TimedLong", self(), []),
    %ok = port_mgr:add_port(PMPid, svc, "Svc", "", []),
    [{pm, PMPid}, {ps, PSPid} | Config];
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
end_per_testcase(destroy_ports, Config) ->
    Pid = ?config(pm, Config),
    port_mgr:stop_server(Pid);
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
    ok = port_mgr:test().


%%-----------------------------------------------------------------------------
%% @doc Tests starting and stopping the manager.
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
    {ok, Pid} = port_mgr:start_link(),
    true = is_process_alive(Pid),
    [] = port_mgr:get_port_names(Pid),
    ok = port_mgr:stop_server(Pid),
    timer:sleep(100),
    false = is_process_alive(Pid).


%%-----------------------------------------------------------------------------
%% @doc Tests adding a datain port.
%% @spec add_datain_port(Config0) ->
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
-spec(add_datain_port([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
add_datain_port(Config) ->
    Pid = ?config(pm, Config),
    ok = port_mgr:add_port(Pid, datain, "DataIn", "TimedLong", self(), []),
    true = lists:member("DataIn", port_mgr:get_port_names(Pid)),
    DataInPort = port_mgr:get_port(Pid, "DataIn"),
    true = is_process_alive(DataInPort).


%%-----------------------------------------------------------------------------
%% @doc Tests adding a dataout port.
%% @spec add_dataout_port(Config0) ->
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
-spec(add_dataout_port([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
add_dataout_port(Config) ->
    Pid = ?config(pm, Config),
    ok = port_mgr:add_port(Pid, dataout, "DataOut", "TimedLong", self(), []),
    true = lists:member("DataOut", port_mgr:get_port_names(Pid)),
    DataOutPort = port_mgr:get_port(Pid, "DataOut"),
    true = is_process_alive(DataOutPort).


%%-----------------------------------------------------------------------------
%% @doc Tests adding a service port.
%% @spec add_svc_port(Config0) ->
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
-spec(add_svc_port([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
add_svc_port(Config) ->
    Pid = ?config(pm, Config),
    ok = port_mgr:add_port(Pid, svc, "Svc", "TimedLong", self(), []),
    true = lists:member("Svc", port_mgr:get_port_names(Pid)),
    SvcPort = port_mgr:get_port(Pid, "Svc"),
    true = is_process_alive(SvcPort).


%%-----------------------------------------------------------------------------
%% @doc Tests removing a datain port.
%% @spec rem_datain_port(Config0) ->
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
-spec(rem_datain_port([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
rem_datain_port(Config) ->
    Pid = ?config(pm, Config),
    true = lists:member("DataIn", port_mgr:get_port_names(Pid)),
    DataInPort = port_mgr:get_port(Pid, "DataIn"),
    ok = port_mgr:rem_port(Pid, "DataIn"),
    false = lists:member("DataIn", port_mgr:get_port_names(Pid)),
    false = is_process_alive(DataInPort).


%%-----------------------------------------------------------------------------
%% @doc Tests removing a dataout port.
%% @spec rem_dataout_port(Config0) ->
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
-spec(rem_dataout_port([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
rem_dataout_port(Config) ->
    Pid = ?config(pm, Config),
    true = lists:member("DataOut", port_mgr:get_port_names(Pid)),
    DataOutPort = port_mgr:get_port(Pid, "DataOut"),
    ok = port_mgr:rem_port(Pid, "DataOut"),
    false = lists:member("DataOut", port_mgr:get_port_names(Pid)),
    false = is_process_alive(DataOutPort).


%%-----------------------------------------------------------------------------
%% @doc Tests removing a service port.
%% @spec rem_svc_port(Config0) ->
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
-spec(rem_svc_port([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
rem_svc_port(Config) ->
    Pid = ?config(pm, Config),
    true = lists:member("Svc", port_mgr:get_port_names(Pid)),
    SvcPort = port_mgr:get_port(Pid, "Svc"),
    ok = port_mgr:rem_port(Pid, "Svc"),
    false = lists:member("Svc", port_mgr:get_port_names(Pid)),
    false = is_process_alive(SvcPort).


%%-----------------------------------------------------------------------------
%% @doc Tests shutting down with ports.
%% @spec destroy_ports(Config0) ->
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
-spec(destroy_ports([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
destroy_ports(Config) ->
    Pid = ?config(pm, Config),
    true = lists:member("DataIn", port_mgr:get_port_names(Pid)),
    true = lists:member("DataOut", port_mgr:get_port_names(Pid)),
    %true = lists:member("Svc", port_mgr:get_port_names(Pid)),
    DataInPort = port_mgr:get_port(Pid, "DataIn"),
    DataOutPort = port_mgr:get_port(Pid, "DataOut"),
    %SvcPort = port_mgr:get_port(Pid, "Svc"),
    link(Pid),
    port_mgr:stop_server(Pid),
    timer:sleep(1000),
    false = is_process_alive(DataInPort),
    false = is_process_alive(DataOutPort).
    %false = is_process_alive(SvcPort).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the port PIDs.
%% @spec get_ports(Config0) ->
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
-spec(get_ports([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_ports(Config) ->
    Pid = ?config(pm, Config),
    true = lists:member("DataIn", port_mgr:get_port_names(Pid)),
    true = lists:member("DataOut", port_mgr:get_port_names(Pid)),
    %true = lists:member("Svc", port_mgr:get_port_names(Pid)),
    DataInPort = port_mgr:get_port(Pid, "DataIn"),
    DataOutPort = port_mgr:get_port(Pid, "DataOut"),
    %SvcPort = port_mgr:get_port(Pid, "Svc"),
    Ports = port_mgr:get_ports(Pid),
    true = lists:member(DataInPort, Ports),
    true = lists:member(DataOutPort, Ports).
    %true = lists:member(SvcPort, Ports).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the port profiles.
%% @spec port_profs(Config0) ->
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
-spec(port_profs([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
port_profs(Config) ->
    Pid = ?config(pm, Config),
    Owner = ?config(owner, Config),
    DIPort = port_mgr:get_port(Pid, "DataIn"),
    DIProf = #port_prof{name="DataIn", interfaces=[], port_ref=DIPort,
        conn_profs=[], owner=Owner,
        props=[{"dataport", [{"data_type", "TimedLong"},
                    {"interface_types", "erlang,corba_cdr"}]},
            {"port", [{"port_type", "DataInPort"}]}]},
    DOPort = port_mgr:get_port(Pid, "DataOut"),
    DOProf = #port_prof{name="DataOut", interfaces=[], port_ref=DOPort,
        conn_profs=[], owner=Owner,
        props=[{"dataport", [{"data_type", "TimedLong"},
                    {"interface_types", "erlang,corba_cdr"}]},
            {"port", [{"port_type", "DataOutPort"}]}]},
    Profs = port_mgr:port_profs(Pid),
    ct:log("~p", [Profs]),
    ct:log("~p", [DIProf]),
    true = lists:member(DIProf, Profs),
    true = lists:member(DOProf, Profs).


