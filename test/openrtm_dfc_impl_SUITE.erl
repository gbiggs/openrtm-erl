%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for OpenRTM_DataFlowComponent_impl.erl.
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
-module(openrtm_dfc_impl_SUITE).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include("idl/RTC.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include("idl/SDOPackage.hrl").
-include("type_specs.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Test server callbacks
%%-----------------------------------------------------------------------------
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
        init_per_group/2, end_per_group/2, init_per_testcase/2,
        end_per_testcase/2]).


%%-----------------------------------------------------------------------------
%% Test cases
%%-----------------------------------------------------------------------------
-export([init_finalize/1, exit_rtc/1, startup_shutdown/1,
        activate_deactivate/1, execute/1, error_reset/1, is_alive/1,
        get_comp_prof/1, get_ports/1, attach_detach_ec/1, get_sdo_id/1,
        get_sdo_type/1, get_configuration/1, register/1]).
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
    [init_finalize, exit_rtc, attach_detach_ec, startup_shutdown,
        activate_deactivate, execute, error_reset, {group, introspection},
        {group, sdo}].


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
    [{introspection, [sequence], [get_comp_prof, get_ports, is_alive]},
        {sdo, [shuffle], [get_sdo_id, get_sdo_type, get_configuration]}].


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
    %application:load(sasl),
    %application:start(sasl),
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
init_per_group(introspection, Config) ->
    start_mock_behv(),
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    RTCObj = rtc:get_corba_obj(FSM),
    rtc:initialize(FSM),
    true = unlink(RTC),
    [{rtc, {RTC, FSM, RTCObj}} | Config];
init_per_group(sdo, Config) ->
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    Obj = rtc:get_corba_obj(FSM),
    true = unlink(RTC),
    [{rtc, {RTC, FSM, Obj}} | Config];
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
end_per_group(introspection, Config) ->
    {RTC, _FSM, _Obj} = ?config(rtc, Config),
    rtc:destroy(RTC);
end_per_group(sdo, Config) ->
    {RTC, _FSM, _Obj} = ?config(rtc, Config),
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
%% @doc Finds the specified object.
%% @spec find_obj(Path) -> object_ref()
%% where
%%      Path = string()
%% @end
-spec(find_obj(Path::string()) -> object_ref()).
%%-----------------------------------------------------------------------------
find_obj(Path) ->
    Path1 = lists:append(["corbaname:iiop:1.2@localhost:2809/NameService#",
            Path]),
    corba:string_to_object(Path1).


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
init_per_testcase(init_finalize, Config) ->
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    Obj = rtc:get_corba_obj(FSM),
    true = unlink(RTC),
    [{rtc, {RTC, FSM, Obj}} | Config];
init_per_testcase(exit_rtc, Config) ->
    start_mock_behv(),
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    RTCObj = rtc:get_corba_obj(FSM),
    rtc:initialize(FSM),
    true = unlink(RTC),
    {ok, ECSup, EC_FSM} = ec:create([]),
    ECObj = ec:get_corba_obj(EC_FSM),
    true = unlink(ECSup),
    [{ec, {ECSup, EC_FSM, ECObj}}, {rtc, {RTC, FSM, RTCObj}} | Config];
init_per_testcase(attach_detach_ec, Config) ->
    start_mock_behv(),
    {ok, RTC1, FSM1} = rtc:create(simple_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    RTCObj1 = rtc:get_corba_obj(FSM1),
    rtc:initialize(FSM1),
    true = unlink(RTC1),
    {ok, RTC2, FSM2} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    RTCObj2 = rtc:get_corba_obj(FSM2),
    rtc:initialize(FSM2),
    true = unlink(RTC2),
    {ok, ECSup, EC_FSM} = ec:create([]),
    ECObj = ec:get_corba_obj(EC_FSM),
    true = unlink(ECSup),
    [{ec, {ECSup, EC_FSM, ECObj}}, {rtc2, {RTC2, FSM2, RTCObj2}},
        {rtc1, {RTC1, FSM1, RTCObj1}} | Config];
init_per_testcase(TestCase, Config) when TestCase == startup_shutdown;
        TestCase == activate_deactivate; TestCase == execute;
        TestCase == error_reset ->
    start_mock_behv(),
    {ok, RTC, FSM} = rtc:create(one_ec_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    RTCObj = rtc:get_corba_obj(FSM),
    rtc:initialize(FSM),
    [on_initialize] = mock_rtc_behv:cbs(),
    true = unlink(RTC),
    [{rtc, {RTC, FSM, RTCObj}} | Config];
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
end_per_testcase(init_finalize, Config) ->
    {RTC, _FSM, _Obj} = ?config(rtc, Config),
    rtc:destroy(RTC);
end_per_testcase(exit_rtc, Config) ->
    {RTC, _FSM, _Obj} = ?config(rtc, Config),
    {ECSup, _EC_FSM, _ECObj} = ?config(ec, Config),
    ok = rtc:destroy(RTC),
    ok = ec:destroy(ECSup),
    stop_mock_behv();
end_per_testcase(attach_detach_ec, Config) ->
    {RTC1, _RTC_FSM1, _RTCObj1} = ?config(rtc1, Config),
    {RTC2, _RTC_FSM2, _RTCObj2} = ?config(rtc2, Config),
    {ECSup, _EC_FSM, _ECObj} = ?config(ec, Config),
    ok = rtc:destroy(RTC1),
    ok = rtc:destroy(RTC2),
    ok = ec:destroy(ECSup),
    stop_mock_behv();
end_per_testcase(TestCase, Config) when TestCase == startup_shutdown;
        TestCase == activate_deactivate; TestCase == execute;
        TestCase == error_reset ->
    {RTC, _FSM, _Obj} = ?config(rtc, Config),
    ok = rtc:destroy(RTC),
    stop_mock_behv();
end_per_testcase(_TestCase, _Config) ->
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests initializing and finalizing a component.
%% @spec init_finalize(Config0) ->
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
-spec(init_finalize(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
init_finalize(Config) ->
    {_RTC, FSM, Obj} = ?config(rtc, Config),
    [] = rtc:get_owned_contexts(FSM),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':initialize(Obj),
    [EC] = rtc:get_owned_contexts(FSM),
    [EC] = rtc:get_participating_contexts(FSM),
    ok = ec:remove_component(EC, FSM),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':finalize(Obj).


%%-----------------------------------------------------------------------------
%% @doc Tests exiting a component.
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
-spec(exit_rtc(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
exit_rtc(Config) ->
    {_RTC, FSM, Obj} = ?config(rtc, Config),
    {_EC_Sup, EC, ECObj} = ?config(ec, Config),
    'RTC_OK' = 'RTC_ExecutionContextService':add_component(ECObj, Obj),
    [OwnedEC] = rtc:get_owned_contexts(FSM),
    ec:start(EC),
    ec:start(OwnedEC),
    'RTC_OK' = 'RTC_ExecutionContextService':activate_component(ECObj, Obj),
    ok = ec:activate_component(OwnedEC, FSM),
    rtc:register_cb(FSM, finalised, cb_maker(finalised, self())),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':exit(Obj),
    finalised = wait_for(1000),
    timer:sleep(100),
    false = is_process_alive(OwnedEC),
    true = ec:is_running(EC),
    [] = ec:get_parts(EC).


%%-----------------------------------------------------------------------------
%% @doc Tests the component response to an EC start/stop.
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
-spec(startup_shutdown(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
startup_shutdown(Config) ->
    {_RTC, FSM, Obj} = ?config(rtc, Config),
    [EC] = 'OpenRTM_DataFlowComponent':get_owned_contexts(Obj),
    Handle = 'OpenRTM_DataFlowComponent':get_context_handle(Obj, EC),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_startup(Obj, Handle),
    [on_startup] = mock_rtc_behv:cbs(),
    on_startup_result = rtc:get_behv_data(FSM),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_rate_changed(Obj, Handle),
    [on_rate_changed] = mock_rtc_behv:cbs(),
    on_rate_changed_result = rtc:get_behv_data(FSM),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_shutdown(Obj, Handle),
    [on_shutdown] = mock_rtc_behv:cbs(),
    on_shutdown_result = rtc:get_behv_data(FSM).


%%-----------------------------------------------------------------------------
%% @doc Tests the component response to being activated and deactivated.
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
-spec(activate_deactivate(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
activate_deactivate(Config) ->
    {_RTC, FSM, Obj} = ?config(rtc, Config),
    [EC] = 'OpenRTM_DataFlowComponent':get_owned_contexts(Obj),
    Handle = 'OpenRTM_DataFlowComponent':get_context_handle(Obj, EC),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_activated(Obj, Handle),
    [on_activated] = mock_rtc_behv:cbs(),
    on_activated_result = rtc:get_behv_data(FSM),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_deactivated(Obj, Handle),
    [on_deactivated] = mock_rtc_behv:cbs(),
    on_deactivated_result = rtc:get_behv_data(FSM).


%%-----------------------------------------------------------------------------
%% @doc Tests the component response to execution events.
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
-spec(execute(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
execute(Config) ->
    {_RTC, FSM, Obj} = ?config(rtc, Config),
    [EC] = 'OpenRTM_DataFlowComponent':get_owned_contexts(Obj),
    Handle = 'OpenRTM_DataFlowComponent':get_context_handle(Obj, EC),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_execute(Obj, Handle),
    [on_execute] = mock_rtc_behv:cbs(),
    on_execute_result = rtc:get_behv_data(FSM),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_state_update(Obj, Handle),
    [on_state_update] = mock_rtc_behv:cbs(),
    on_state_update_result = rtc:get_behv_data(FSM).


%%-----------------------------------------------------------------------------
%% @doc Tests the component response to error events.
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
-spec(error_reset(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
error_reset(Config) ->
    {_RTC, FSM, Obj} = ?config(rtc, Config),
    [EC] = 'OpenRTM_DataFlowComponent':get_owned_contexts(Obj),
    Handle = 'OpenRTM_DataFlowComponent':get_context_handle(Obj, EC),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_aborting(Obj, Handle),
    [on_aborting] = mock_rtc_behv:cbs(),
    on_aborting_result = rtc:get_behv_data(FSM),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_error(Obj, Handle),
    [on_error] = mock_rtc_behv:cbs(),
    on_error_result = rtc:get_behv_data(FSM),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':on_reset(Obj, Handle),
    [on_reset] = mock_rtc_behv:cbs(),
    on_reset_result = rtc:get_behv_data(FSM).


%%-----------------------------------------------------------------------------
%% @doc Tests the component's is_alive response.
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
-spec(is_alive(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
is_alive(Config) ->
    {_RTC, RTC_FSM, RTCObj} = ?config(rtc, Config),
    [EC] = rtc:get_owned_contexts(RTC_FSM),
    ECObj = ec:get_corba_obj(EC),
    ok = rtc:shift_to_state(RTC_FSM, created),
    created = dataflow_rtc:state(RTC_FSM),
    false = 'OpenRTM_DataFlowComponent':is_alive(RTCObj, ECObj),
    ok = rtc:shift_to_state(RTC_FSM, init_wait),
    init_wait = dataflow_rtc:state(RTC_FSM),
    false = 'OpenRTM_DataFlowComponent':is_alive(RTCObj, ECObj),
    ok = rtc:shift_to_state(RTC_FSM, alive),
    alive = dataflow_rtc:state(RTC_FSM),
    true = 'OpenRTM_DataFlowComponent':is_alive(RTCObj, ECObj),
    false = 'OpenRTM_DataFlowComponent':is_alive(RTCObj, ?ORBER_NIL_OBJREF),
    ok = rtc:shift_to_state(RTC_FSM, finalise_wait),
    finalise_wait = dataflow_rtc:state(RTC_FSM),
    false = 'OpenRTM_DataFlowComponent':is_alive(RTCObj, ECObj),
    ok = rtc:shift_to_state(RTC_FSM, exit_wait),
    exit_wait = dataflow_rtc:state(RTC_FSM),
    false = 'OpenRTM_DataFlowComponent':is_alive(RTCObj, ECObj),
    ok = rtc:shift_to_state(RTC_FSM, destroyed),
    destroyed = dataflow_rtc:state(RTC_FSM),
    false = 'OpenRTM_DataFlowComponent':is_alive(RTCObj, ECObj).


%%-----------------------------------------------------------------------------
%% @doc Tests getting a component's profile.
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
-spec(get_comp_prof(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_comp_prof(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    ExpectedProf = #'RTC_ComponentProfile'{instance_name="mock_rtc_behv0",
        type_name="mock_rtc_behv",
        description="Fake RTC.",
        version="1.0",
        vendor="Me",
        category="Test",
        port_profiles=[],
        parent=?ORBER_NIL_OBJREF,
        properties=[]},
    ExpectedProf = 'OpenRTM_DataFlowComponent':get_component_profile(Obj).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the port objects.
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
-spec(get_ports(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_ports(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    [] = 'OpenRTM_DataFlowComponent':get_ports(Obj).


%%-----------------------------------------------------------------------------
%% @doc Tests attaching to and detaching from an EC.
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
-spec(attach_detach_ec(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
attach_detach_ec(Config) ->
    % For an RTC with no ECs
    {_RTC1, _RTC_FSM1, RTCObj1} = ?config(rtc1, Config),
    {_RTC2, _RTC_FSM2, RTCObj2} = ?config(rtc2, Config),
    {_EC_Sup, _EC, ECObj} = ?config(ec, Config),
    'PRECONDITION_NOT_MET' =
        'OpenRTM_DataFlowComponent':detach_context(RTCObj1, ECObj),
    H = 'OpenRTM_DataFlowComponent':attach_context(RTCObj1, ECObj),
    [] = 'OpenRTM_DataFlowComponent':get_owned_contexts(RTCObj1),
    [ECObj] = 'OpenRTM_DataFlowComponent':get_participating_contexts(RTCObj1),
    ECObj = 'OpenRTM_DataFlowComponent':get_context(RTCObj1, H),
    % For an RTC with owned ECs
    H2 = 'OpenRTM_DataFlowComponent':attach_context(RTCObj2, ECObj),
    [OwnedEC] = 'OpenRTM_DataFlowComponent':get_owned_contexts(RTCObj2),
    Parts = 'OpenRTM_DataFlowComponent':get_participating_contexts(RTCObj2),
    2 = length(Parts),
    true = lists:member(OwnedEC, Parts),
    true = lists:member(ECObj, Parts),
    ECObj = 'OpenRTM_DataFlowComponent':get_context(RTCObj2, H2),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':detach_context(RTCObj1, ECObj),
    'RTC_OK' = 'OpenRTM_DataFlowComponent':detach_context(RTCObj2, ECObj),
    0 = length('OpenRTM_DataFlowComponent':get_participating_contexts(RTCObj1)),
    1 = length('OpenRTM_DataFlowComponent':get_participating_contexts(RTCObj2)).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the SDO ID of a component.
%% @spec get_sdo_id(Config0) ->
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
-spec(get_sdo_id(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_sdo_id(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    "mock_rtc_behv0" = 'OpenRTM_DataFlowComponent':get_sdo_id(Obj).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the SDO type of a component.
%% @spec get_sdo_type(Config0) ->
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
-spec(get_sdo_type(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_sdo_type(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    "mock_rtc_behv" = 'OpenRTM_DataFlowComponent':get_sdo_type(Obj).


%%-----------------------------------------------------------------------------
%% @doc Tests getting a component's configuration sets.
%% @spec get_configuration(Config0) ->
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
-spec(get_configuration(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_configuration(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    Cfg = 'OpenRTM_DataFlowComponent':get_configuration(Obj),
    true = corba_object:is_a(Cfg, 'SDOPackage_Configuration':typeID()).


%%-----------------------------------------------------------------------------
%% @doc Tests registering on a naming context.
%% @spec register(Config0) ->
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
-spec(register(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
register(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    Obj = find_obj("mock_rtc_behv0.rtc"),
    true = corba_object:is_a('OpenRTM_DataFlowComponent',
        'OpenRTM_DataFlowComponent':typeID()).

