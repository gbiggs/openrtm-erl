%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for dataflow_rtc.erl via rtc.erl.
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
-module(dataflow_rtc_SUITE).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("type_specs.hrl").
-include("rtc.hrl").
-include("log.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Test server callbacks
%%-----------------------------------------------------------------------------
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
        end_per_group/2, init_per_testcase/2, end_per_testcase/2]).


%%-----------------------------------------------------------------------------
%% Test cases
%%-----------------------------------------------------------------------------
-export([eunit_tests/1, create_destroy/1, callbacks/1, get_corba_obj/1,
        is_alive/1, exit_rtc/1, type/1, init_no_ecs/1, init_one_ec/1,
        init_multi_ecs/1, attach_detach_ec/1, startup_shutdown/1,
        activate_deactivate/1, execute/1, error_reset/1, get_comp_prof/1,
        get_ports/1]).
-export([num_sfx_gen/0]).


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
    [eunit_tests, create_destroy, is_alive, exit_rtc, init_no_ecs, init_one_ec,
        init_multi_ecs, attach_detach_ec, startup_shutdown,
        activate_deactivate, execute, error_reset, {group, simple_cfg},
        get_comp_prof, get_ports].


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
    [{simple_cfg, [sequence], [type, get_corba_obj, callbacks]}].


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
init_per_group(simple_cfg, Config) ->
    {ok, RTC, FSM} = rtc:create(simple_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    true = unlink(RTC),
    [{rtc, {RTC, FSM}} | Config];
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
end_per_group(simple_cfg, Config) ->
    {RTC, _FSM} = ?config(rtc, Config),
    rtc:destroy(RTC);
end_per_group(_GroupName, _Config) ->
    ok.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create a simple configuration.
%% @spec simple_cfg() -> config()
%% @end
-spec(simple_cfg() -> config()).
%%-----------------------------------------------------------------------------
simple_cfg() ->
    Cfg1 = config:set_value("implementation_id", "MockRTCBehaviour",
        config:empty_conf()),
    Cfg2 = config:set_value("type_name", "mock_rtc_behv", Cfg1),
    Cfg3 = config:set_value("description", "Fake RTC.", Cfg2),
    Cfg4 = config:set_value("version", "1.0", Cfg3),
    Cfg5 = config:set_value("vendor", "Me", Cfg4),
    Cfg6 = config:set_value("category", "Test", Cfg5),
    Cfg7 = config:set_value("activity_type", "DataFlowComponent", Cfg6),
    Cfg8 = config:set_value("max_instance", "1", Cfg7),
    Cfg9 = config:set_value("language", "Erlang", Cfg8),
    Cfg10 = config:set_value("lang_type", "compile", Cfg9),
    config:set_value("corba", [{"nameservers", "127.0.0.1:28090"}], Cfg10).


%%-----------------------------------------------------------------------------
%% @doc Create a configuration with one EC.
%% @spec one_ec_cfg() -> config()
%% @end
-spec(one_ec_cfg() -> config()).
%%-----------------------------------------------------------------------------
one_ec_cfg() ->
    config:set_value("exec_cxts",
        [{"ec1", [{"type", "periodic"}, {"rate", "1.0"}]}],
        simple_cfg()).


%%-----------------------------------------------------------------------------
%% @doc Create a configuration with three ECs.
%% @spec three_ecs_cfg() -> config()
%% @end
-spec(three_ecs_cfg() -> config()).
%%-----------------------------------------------------------------------------
three_ecs_cfg() ->
    config:set_value("exec_cxts",
        [{"ec1", [{"type", "periodic"}, {"rate", "1.0"}]},
            {"ec2", [{"type", "periodic"}, {"rate", "2.0"}]},
            {"ec3", [{"type", "periodic"}, {"rate", "3.0"}]}],
        simple_cfg()).


%%-----------------------------------------------------------------------------
%% @doc Wait for a message sent by a callback.
%% @spec wait_for(Timeout) -> Result
%% where
%%      Timeout = pos_integer() | infinity
%%      Result = any()
%% @end
-spec(wait_for(Timeout::pos_integer() | infinity) -> Result::any()).
%%-----------------------------------------------------------------------------
wait_for(Timeout) ->
    receive
        Msg ->
            Msg
    after Timeout ->
        failed
    end.


%%-----------------------------------------------------------------------------
%% @doc Return a closure constructor around a message and a destination.
%% @spec cb_maker(Msg, Pid) -> fun()
%% where
%%      Msg = any()
%%      Pid = pid()
%% @end
-spec(cb_maker(Msg::any(), Pid::pid()) -> fun()).
%%-----------------------------------------------------------------------------
cb_maker(Msg, Pid) ->
    fun() ->
        Pid ! Msg
    end.


%%-----------------------------------------------------------------------------
%% @doc Check an EC.
%% @spec check_ec(EC, RTC, IsRunning, Rate, Kind) -> ok
%% where
%%      EC = pid()
%%      RTC = pid()
%%      IsRunning = boolean()
%%      Rate = float(),
%%      Kind = periodic | fsm
%% @end
-spec(check_ec(EC::pid(), RTC::pid(), IsRunning::boolean(), Rate::float(),
        Kind::periodic | fsm) -> ok).
%%-----------------------------------------------------------------------------
check_ec(EC, RTC, IsRunning, Rate, Kind) ->
    true = is_pid(EC),
    IsRunning = ec:is_running(EC),
    Rate = ec:get_rate(EC),
    Kind = ec:get_kind(EC),
    RTC = ec:get_owner(EC),
    Handle = rtc:get_context_handle(RTC, EC),
    EC = rtc:get_context(RTC, Handle),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Start a process to record callback histories.
%% @spec start_mock_behv() -> ok
%% @end
-spec(start_mock_behv() -> ok).
%%-----------------------------------------------------------------------------
start_mock_behv() ->
    {ok, _Pid} = mock_rtc_behv:start_link(),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Stop a process that is recording callback histories.
%% @spec stop_mock_behv() -> Pid
%% where
%%      Pid = pid()
%% @end
-spec(stop_mock_behv() -> ok).
%%-----------------------------------------------------------------------------
stop_mock_behv() ->
    mock_rtc_behv:stop_server().


%%-----------------------------------------------------------------------------
%% @doc Function to return a textual number.
%% @spec num_sfx_gen() -> "0".
%% @end
-spec(num_sfx_gen() -> [pos_integer()]).
%%-----------------------------------------------------------------------------
num_sfx_gen() ->
    "0".


%%-----------------------------------------------------------------------------
%% @doc Check if two port profiles are equal except for the PIDs.
%% @spec portprofs_mostly_equal(Prof1, Prof2) -> boolean()
%% where
%%      Prof1 = #port_prof{}
%%      Prof1 = #port_prof{}
%% @end
-spec(portprofs_mostly_equal(Prof1::#port_prof{}, Prof2::#port_prof{}) ->
    boolean()).
%%-----------------------------------------------------------------------------
portprofs_mostly_equal(#port_prof{name=N, interfaces=I, conn_profs=C1,
        props=Props1},
        #port_prof{name=N, interfaces=I, conn_profs=C2, props=Props2}) ->
    true = config:compare(Props1, Props2, false, false),
    true = compare_conn_prof_lists(C1, C2, true);
portprofs_mostly_equal(_Prof1, _Prof2) ->
    false.


%%-----------------------------------------------------------------------------
%% @doc Check if two sets of connection profiles are equal.
%% @spec compare_conn_prof_lists(C1, C2, IgMissing) -> boolean()
%% where
%%      C1 = C2 = [#conn_prof{}]
%%      IgMissing = boolean()
%% @end
-spec(compare_conn_prof_lists(C1::[#conn_prof{}], C2::[#conn_prof{}],
        IgMissing::boolean()) ->
    boolean()).
%%-----------------------------------------------------------------------------
compare_conn_prof_lists([], _, _) ->
    true;
compare_conn_prof_lists([H|T], C2, IgnoreMissing) ->
    case cp_in_list(H, C2, IgnoreMissing)
        of true ->
            compare_conn_prof_lists(T, C2, IgnoreMissing)
         ; false ->
            false
    end.

-spec(cp_in_list(CP::#conn_prof{}, L::[#conn_prof{}], IgMissing::boolean()) ->
    boolean()).
cp_in_list(_, [], _) ->
    false;
cp_in_list(CP, [H|T], IgMissing) ->
    case comp_cps(CP, H, IgMissing)
        of true ->
            true
         ; false ->
            cp_in_list(CP, T, IgMissing)
    end.

-spec(comp_cps(#conn_prof{}, #conn_prof{}, IgMissing::boolean()) -> boolean()).
comp_cps(#conn_prof{id=Id, ports=P, props=Props1},
        #conn_prof{id=Id, ports=P, props=Props2}, IgMissing) ->
    config:compare(Props1, Props2, IgMissing, false).


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
init_per_testcase(exit_rtc, Config) ->
    start_mock_behv(),
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun dataflow_rtc_SUITE:num_sfx_gen/0,
        mock_rtc_behv),
    true = unlink(RTC),
    {ok, EC_Sup, EC_FSM} = ec:create([]),
    true = unlink(EC_Sup),
    [{ec, {EC_Sup, EC_FSM}}, {rtc, {RTC, FSM}} | Config];
init_per_testcase(startup_shutdown, Config) ->
    start_mock_behv(),
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun dataflow_rtc_SUITE:num_sfx_gen/0,
        mock_rtc_behv),
    true = unlink(RTC),
    [{rtc, {RTC, FSM}} | Config];
init_per_testcase(activate_deactivate, Config) ->
    start_mock_behv(),
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    true = unlink(RTC),
    [{rtc, {RTC, FSM}} | Config];
init_per_testcase(execute, Config) ->
    start_mock_behv(),
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    true = unlink(RTC),
    [{rtc, {RTC, FSM}} | Config];
init_per_testcase(error_reset, Config) ->
    start_mock_behv(),
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    true = unlink(RTC),
    [{rtc, {RTC, FSM}} | Config];
init_per_testcase(is_alive, Config) ->
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    true = unlink(RTC),
    [{rtc, {RTC, FSM}} | Config];
init_per_testcase(get_comp_prof, Config) ->
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv_ports),
    true = unlink(RTC),
    ok = rtc:initialize(FSM),
    [{rtc, {RTC, FSM}} | Config];
init_per_testcase(get_ports, Config) ->
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv_ports),
    true = unlink(RTC),
    ok = rtc:initialize(FSM),
    [{rtc, {RTC, FSM}} | Config];
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
end_per_testcase(exit_rtc, Config) ->
    {RTC, _FSM} = ?config(rtc, Config),
    {EC_Sup, _EC_FSM} = ?config(ec, Config),
    ok = rtc:destroy(RTC),
    ok = ec:destroy(EC_Sup),
    stop_mock_behv();
end_per_testcase(startup_shutdown, Config) ->
    {RTC, _FSM} = ?config(rtc, Config),
    ok = rtc:destroy(RTC),
    stop_mock_behv();
end_per_testcase(activate_deactivate, Config) ->
    {RTC, _FSM} = ?config(rtc, Config),
    ok = rtc:destroy(RTC),
    stop_mock_behv();
end_per_testcase(execute, Config) ->
    {RTC, _FSM} = ?config(rtc, Config),
    ok = rtc:destroy(RTC),
    stop_mock_behv();
end_per_testcase(error_reset, Config) ->
    {RTC, _FSM} = ?config(rtc, Config),
    ok = rtc:destroy(RTC),
    stop_mock_behv();
end_per_testcase(is_alive, Config) ->
    {RTC, _FSM} = ?config(rtc, Config),
    ok = rtc:destroy(RTC);
end_per_testcase(get_comp_prof, Config) ->
    {RTC, _FSM} = ?config(rtc, Config),
    ok = rtc:destroy(RTC);
end_per_testcase(get_ports, Config) ->
    {RTC, _FSM} = ?config(rtc, Config),
    ok = rtc:destroy(RTC);
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
    ok = dataflow_rtc:test().


%%-----------------------------------------------------------------------------
%% @doc Tests creating and destroying a basic RTC.
%% @spec create_destroy(Config0) ->
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
-spec(create_destroy([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
create_destroy(_Config) ->
    {ok, RTC, FSM} = rtc:create(simple_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    created = dataflow_rtc:state(FSM),
    ok = rtc:destroy(RTC).


%%-----------------------------------------------------------------------------
%% @doc Tests the is_alive function in all states.
%% @spec is_alive(Config0) ->
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
-spec(is_alive([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
is_alive(Config) ->
    {_RTC, FSM} = ?config(rtc, Config),
    created = dataflow_rtc:state(FSM),
    false = rtc:is_alive(FSM, none),
    ok = rtc:shift_to_state(FSM, init_wait),
    init_wait = dataflow_rtc:state(FSM),
    false = rtc:is_alive(FSM, none),
    ok = rtc:shift_to_state(FSM, alive),
    alive = dataflow_rtc:state(FSM),
    false = rtc:is_alive(FSM, none),
    ok = rtc:shift_to_state(FSM, finalise_wait),
    finalise_wait = dataflow_rtc:state(FSM),
    false = rtc:is_alive(FSM, none),
    ok = rtc:shift_to_state(FSM, exit_wait),
    exit_wait = dataflow_rtc:state(FSM),
    false = rtc:is_alive(FSM, none),
    ok = rtc:shift_to_state(FSM, destroyed),
    destroyed = dataflow_rtc:state(FSM),
    false = rtc:is_alive(FSM, none),
    ok = rtc:shift_to_state(FSM, created),
    ok = rtc:initialize(FSM),
    [EC] = rtc:get_owned_contexts(FSM),
    true = rtc:is_alive(FSM, EC).


%%-----------------------------------------------------------------------------
%% @doc Tests calling exit on an RTC.
%% @spec exit_rtc(Config0) ->
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
-spec(exit_rtc([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
exit_rtc(Config) ->
    {_RTC, FSM} = ?config(rtc, Config),
    {_EC_Sup, EC} = ?config(ec, Config),
    Ref = rtc:register_cb(FSM, initialised, cb_maker(initialised, self())),
    case Ref
        of {error, bad_event} ->
            exit(failed_reg_cb)
         ; _ ->
            ok
    end,
    ok = rtc:initialize(FSM),
    [on_initialize] = mock_rtc_behv:cbs(),
    ok = ec:add_component(EC, FSM),
    initialised = wait_for(1000),
    [OwnedEC] = rtc:get_owned_contexts(FSM),
    ec:start(EC),
    ec:start(OwnedEC),
    Ref2 = rtc:register_cb(FSM, finalised, cb_maker(finalised, self())),
    case Ref2
        of {error, bad_event} ->
            exit(failed_reg_cb)
         ; _ ->
            ok
    end,
    ok = rtc:exit(FSM),
    finalised = wait_for(1000),
    timer:sleep(100),
    false = is_process_alive(OwnedEC),
    true = ec:is_running(EC),
    [] = ec:get_parts(EC).


%%-----------------------------------------------------------------------------
%% @doc Tests type.
%% @spec type(Config0) ->
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
-spec(type([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
type(Config) ->
    {_RTC, FSM} = ?config(rtc, Config),
    dataflow = rtc:type(FSM).


%%-----------------------------------------------------------------------------
%% @doc Tests initialising and finalising an RTC with no default ECs.
%% @spec init_no_ecs(Config0) ->
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
-spec(init_no_ecs([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
init_no_ecs(_Config) ->
    {ok, RTC, FSM} = rtc:create(simple_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    ok = rtc:initialize(FSM),
    [] = rtc:get_owned_contexts(FSM),
    [] = rtc:get_participating_contexts(FSM),
    ok = rtc:finalize(FSM),
    timer:sleep(100),
    ok = rtc:destroy(RTC).


%%-----------------------------------------------------------------------------
%% @doc Tests initialising and finalising an RTC with one default EC.
%% @spec init_one_ec(Config0) ->
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
-spec(init_one_ec([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
init_one_ec(_Config) ->
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    ok = rtc:initialize(FSM),
    [EC] = rtc:get_owned_contexts(FSM),
    ok = check_ec(EC, FSM, false, 1.0, periodic),
    [EC] = rtc:get_participating_contexts(FSM),
    ok = ec:remove_component(EC, FSM),
    ok = rtc:finalize(FSM),
    timer:sleep(100),
    ok = rtc:destroy(RTC).


%%-----------------------------------------------------------------------------
%% @doc Tests initialising and finalising an RTC with multiple default ECs.
%% @spec init_multi_ecs(Config0) ->
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
-spec(init_multi_ecs([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
init_multi_ecs(_Config) ->
    {ok, RTC, FSM} = rtc:create(three_ecs_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    ok = rtc:initialize(FSM),
    [EC1, EC2, EC3] = rtc:get_owned_contexts(FSM),
    ok = check_ec(EC1, FSM, false, 3.0, periodic),
    ok = check_ec(EC2, FSM, false, 2.0, periodic),
    ok = check_ec(EC3, FSM, false, 1.0, periodic),
    Parts = rtc:get_participating_contexts(FSM),
    3 = length(Parts),
    true = lists:member(EC1, Parts),
    true = lists:member(EC2, Parts),
    true = lists:member(EC3, Parts),
    ok = ec:remove_component(EC1, FSM),
    ok = ec:remove_component(EC2, FSM),
    ok = ec:remove_component(EC3, FSM),
    ok = rtc:finalize(FSM),
    timer:sleep(100),
    ok = rtc:destroy(RTC).


%%-----------------------------------------------------------------------------
%% @doc Tests attaching and detaching an EC.
%% @spec attach_detach_ec(Config0) ->
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
-spec(attach_detach_ec([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
attach_detach_ec(_Config) ->
    % For an RTC with no ECs
    {ok, RTC, FSM} = rtc:create(simple_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    {ok, EC_Sup, EC_FSM} = ec:create(config:empty_conf()),
    precon_not_met = rtc:detach_context(FSM, EC_FSM),
    H = rtc:attach_context(FSM, EC_FSM),
    [] = rtc:get_owned_contexts(FSM),
    [EC_FSM] = rtc:get_participating_contexts(FSM),
    EC_FSM = rtc:get_context(FSM, H),
    % For an RTC with owned ECs
    {ok, RTC2, FSM2} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    ok = rtc:initialize(FSM2),
    H2 = rtc:attach_context(FSM2, EC_FSM),
    [OwnedEC] = rtc:get_owned_contexts(FSM2),
    Parts = rtc:get_participating_contexts(FSM2),
    2 = length(Parts),
    true = lists:member(OwnedEC, Parts),
    true = lists:member(EC_FSM, Parts),
    EC_FSM = rtc:get_context(FSM2, H2),
    ok = rtc:detach_context(FSM, EC_FSM),
    ok = rtc:detach_context(FSM2, EC_FSM),
    0 = length(rtc:get_participating_contexts(FSM)),
    1 = length(rtc:get_participating_contexts(FSM2)),
    ok = rtc:destroy(RTC),
    ok = rtc:destroy(RTC2),
    ok = ec:destroy(EC_Sup).


%%-----------------------------------------------------------------------------
%% @doc Tests starting up and shutting down the RTC in an EC.
%% @spec startup_shutdown(Config0) ->
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
-spec(startup_shutdown([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
startup_shutdown(Config) ->
    {_RTC, FSM} = ?config(rtc, Config),
    ok = rtc:initialize(FSM),
    [on_initialize] = mock_rtc_behv:cbs(),
    [EC] = rtc:get_owned_contexts(FSM),
    Handle = rtc:get_context_handle(FSM, EC),
    ok = rtc:on_startup(FSM, Handle),
    [on_startup] = mock_rtc_behv:cbs(),
    on_startup_result = rtc:get_behv_data(FSM),
    ok = rtc:on_rate_changed(FSM, Handle),
    [on_rate_changed] = mock_rtc_behv:cbs(),
    on_rate_changed_result = rtc:get_behv_data(FSM),
    ok = rtc:on_shutdown(FSM, Handle),
    [on_shutdown] = mock_rtc_behv:cbs(),
    on_shutdown_result = rtc:get_behv_data(FSM).


%%-----------------------------------------------------------------------------
%% @doc Tests running through an activate/deactivate cycle.
%% @spec activate_deactivate(Config0) ->
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
-spec(activate_deactivate([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
activate_deactivate(Config) ->
    {_RTC, FSM} = ?config(rtc, Config),
    ok = rtc:initialize(FSM),
    [on_initialize] = mock_rtc_behv:cbs(),
    [EC] = rtc:get_owned_contexts(FSM),
    Handle = rtc:get_context_handle(FSM, EC),
    ok = rtc:on_activated(FSM, Handle),
    [on_activated] = mock_rtc_behv:cbs(),
    on_activated_result = rtc:get_behv_data(FSM),
    ok = rtc:on_deactivated(FSM, Handle),
    [on_deactivated] = mock_rtc_behv:cbs(),
    on_deactivated_result = rtc:get_behv_data(FSM).


%%-----------------------------------------------------------------------------
%% @doc Tests the execution callbacks.
%% @spec execute(Config0) ->
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
-spec(execute([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
execute(Config) ->
    {_RTC, FSM} = ?config(rtc, Config),
    ok = rtc:initialize(FSM),
    [on_initialize] = mock_rtc_behv:cbs(),
    [EC] = rtc:get_owned_contexts(FSM),
    Handle = rtc:get_context_handle(FSM, EC),
    ok = rtc:on_execute(FSM, Handle),
    [on_execute] = mock_rtc_behv:cbs(),
    on_execute_result = rtc:get_behv_data(FSM),
    ok = rtc:on_state_update(FSM, Handle),
    [on_state_update] = mock_rtc_behv:cbs(),
    on_state_update_result = rtc:get_behv_data(FSM).


%%-----------------------------------------------------------------------------
%% @doc Tests the error callbacks.
%% @spec error_reset(Config0) ->
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
-spec(error_reset([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
error_reset(Config) ->
    {_RTC, FSM} = ?config(rtc, Config),
    ok = rtc:initialize(FSM),
    [on_initialize] = mock_rtc_behv:cbs(),
    [EC] = rtc:get_owned_contexts(FSM),
    Handle = rtc:get_context_handle(FSM, EC),
    ok = rtc:on_aborting(FSM, Handle),
    [on_aborting] = mock_rtc_behv:cbs(),
    on_aborting_result = rtc:get_behv_data(FSM),
    ok = rtc:on_error(FSM, Handle),
    [on_error] = mock_rtc_behv:cbs(),
    on_error_result = rtc:get_behv_data(FSM),
    ok = rtc:on_reset(FSM, Handle),
    [on_reset] = mock_rtc_behv:cbs(),
    on_reset_result = rtc:get_behv_data(FSM).


%%-----------------------------------------------------------------------------
%% @doc Tests registering a callback.
%% @spec callbacks(Config0) ->
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
-spec(callbacks([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
callbacks(Config) ->
    {_RTC, FSM} = ?config(rtc, Config),
    ok = rtc:shift_to_state(FSM, alive),
    Ref = rtc:register_cb(FSM, finalised, cb_maker(cb_called, self())),
    ok = rtc:finalize(FSM),
    cb_called = wait_for(1000),
    ok = rtc:shift_to_state(FSM, alive),
    ok = rtc:finalize(FSM),
    cb_called = wait_for(1000),
    ok = rtc:unregister_cb(FSM, Ref),
    ok = rtc:shift_to_state(FSM, alive),
    ok = rtc:finalize(FSM),
    failed = wait_for(1000),
    {error, bad_event} = rtc:register_cb(FSM, blurgle,
        cb_maker(cb_called, self())),
    {error, no_cb} = rtc:unregister_cb(FSM, Ref).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the CORBA object.
%% @spec get_corba_obj(Config0) ->
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
-spec(get_corba_obj([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_corba_obj(Config) ->
    {_RTC, FSM} = ?config(rtc, Config),
    Obj = rtc:get_corba_obj(FSM),
    true = corba_object:is_a(Obj, 'OpenRTM_DataFlowComponent':typeID()).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the component profile.
%% @spec get_comp_prof(Config0) ->
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
-spec(get_comp_prof([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_comp_prof(Config) ->
    {RTC, FSM} = ?config(rtc, Config),
    DIProf = #port_prof{name="DataIn", interfaces=[],
        conn_profs=[], owner=FSM,
        props=[{"dataport", [{"data_type", "TimedLong"},
                    {"interface_types", "erlang,corba_cdr"}]},
            {"port", [{"port_type", "DataInPort"}]}]},
    DOProf = #port_prof{name="DataOut", interfaces=[],
        conn_profs=[], owner=FSM,
        props=[{"dataport", [{"data_type", "TimedLong"},
                    {"interface_types", "erlang,corba_cdr"}]},
            {"port", [{"port_type", "DataOutPort"}]}]},
    #comp_prof{inst_name="mock_rtc_behv0", type_name="mock_rtc_behv",
        desc="Fake RTC.", ver="1.0", vendor="Me", category="Test", parent=nil,
        props=[], port_profs=PProfs} = rtc:get_component_profile(FSM),
    [PP1, PP2] = PProfs,
    true = portprofs_mostly_equal(DOProf, PP1),
    true = portprofs_mostly_equal(DIProf, PP2).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the ports from the component.
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
    {_RTC, FSM} = ?config(rtc, Config),
    [P1, P2] = rtc:get_ports(FSM),
    DIProf = #port_prof{name="DataIn", interfaces=[],
        conn_profs=[], owner=FSM,
        props=[{"dataport", [{"data_type", "TimedLong"},
                    {"interface_types", "erlang,corba_cdr"}]},
            {"port", [{"port_type", "DataInPort"}]}]},
    true = portprofs_mostly_equal(DIProf, portsvc:get_port_profile(P2)),
    DOProf = #port_prof{name="DataOut", interfaces=[],
        conn_profs=[], owner=FSM,
        props=[{"dataport", [{"data_type", "TimedLong"},
                    {"interface_types", "erlang,corba_cdr"}]},
            {"port", [{"port_type", "DataOutPort"}]}]},
    true = portprofs_mostly_equal(DOProf, portsvc:get_port_profile(P1)).

