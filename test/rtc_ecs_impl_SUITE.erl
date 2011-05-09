%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for RTC_ExecutionContextService_impl.erl.
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
-module(rtc_ecs_impl_SUITE).


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
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
        end_per_testcase/2]).


%%-----------------------------------------------------------------------------
%% Test cases
%%-----------------------------------------------------------------------------
-export([get_profile/1, start_stop/1, get_set_rate/1, get_kind/1,
        add_rem_comp/1, act_deact_comp/1, reset_comp/1]).


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
    [get_profile, start_stop, get_set_rate, get_kind, add_rem_comp,
        act_deact_comp, reset_comp].


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
init_per_testcase(Case, Config) when Case == get_profile; Case == start_stop;
        Case == get_set_rate; Case == get_kind; Case == add_rem_comp;
        Case == act_deact_comp; Case == reset_comp ->
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
end_per_testcase(Case, Config) when Case == get_profile; Case == start_stop;
        Case == get_set_rate; Case == get_kind; Case == add_rem_comp;
        Case == act_deact_comp; Case == reset_comp ->
    {RTC, _FSM, _Obj} = ?config(rtc, Config),
    {ECSup, _EC_FSM, _ECObj} = ?config(ec, Config),
    ok = rtc:destroy(RTC),
    ok = ec:destroy(ECSup),
    stop_mock_behv();
end_per_testcase(_TestCase, _Config) ->
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests getting an EC's profile.
%% @spec get_profile(Config0) ->
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
-spec(get_profile(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_profile(Config) ->
    {_RTC, RTC_FSM, RTCObj} = ?config(rtc, Config),
    {_ECSup, _EC_FSM, ECObj} = ?config(ec, Config),
    Expected = #'RTC_ExecutionContextProfile'{kind='PERIODIC',
        rate=1.0, owner=?ORBER_NIL_OBJREF, participants=[],
        properties=[]},
    Expected = 'RTC_ExecutionContextService':get_profile(ECObj),
    [EC_FSM2] = rtc:get_owned_contexts(RTC_FSM),
    ECObj2 = ec:get_corba_obj(EC_FSM2),
    Expected2 = #'RTC_ExecutionContextProfile'{kind='PERIODIC',
        rate=1.0, owner=RTCObj, participants=[RTCObj],
        properties=nvlist:from_list([{"type", "periodic"}, {"rate", "1.0"}])},
    Expected2 = 'RTC_ExecutionContextService':get_profile(ECObj2).


%%-----------------------------------------------------------------------------
%% @doc Tests starting and stopping an EC.
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
-spec(start_stop(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
start_stop(Config) ->
    {_ECSup, _EC_FSM, Obj} = ?config(ec, Config),
    'RTC_OK' = 'RTC_ExecutionContextService':start(Obj),
    true = 'RTC_ExecutionContextService':is_running(Obj),
    'RTC_OK' = 'RTC_ExecutionContextService':stop(Obj),
    false = 'RTC_ExecutionContextService':is_running(Obj).


%%-----------------------------------------------------------------------------
%% @doc Tests getting and setting an EC's rate.
%% @spec get_set_rate(Config0) ->
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
-spec(get_set_rate(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_set_rate(Config) ->
    {_ECSup, _EC_FSM, Obj} = ?config(ec, Config),
    1.0 = 'RTC_ExecutionContextService':get_rate(Obj),
    'RTC_OK' = 'RTC_ExecutionContextService':set_rate(Obj, 29.97),
    29.97 = 'RTC_ExecutionContextService':get_rate(Obj),
    'BAD_PARAMETER' = 'RTC_ExecutionContextService':set_rate(Obj, -100.0),
    'RTC_OK' = 'RTC_ExecutionContextService':start(Obj),
    'RTC_OK' = 'RTC_ExecutionContextService':set_rate(Obj, 15.5),
    15.5 = 'RTC_ExecutionContextService':get_rate(Obj),
    'RTC_OK' = 'RTC_ExecutionContextService':stop(Obj).


%%-----------------------------------------------------------------------------
%% @doc Tests getting an EC's kind.
%% @spec get_kind(Config0) ->
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
-spec(get_kind(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_kind(Config) ->
    {_ECSup, _EC_FSM, Obj} = ?config(ec, Config),
    'PERIODIC' = 'RTC_ExecutionContextService':get_kind(Obj).


%%-----------------------------------------------------------------------------
%% @doc Tests adding and removing a component.
%% @spec add_rem_comp(Config0) ->
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
-spec(add_rem_comp(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
add_rem_comp(Config) ->
    {_RTC, _FSM, RTCObj} = ?config(rtc, Config),
    {_ECSup, _EC_FSM, ECObj} = ?config(ec, Config),
    'RTC_OK' = 'RTC_ExecutionContextService':add_component(ECObj, RTCObj),
    #'RTC_ExecutionContextProfile'{participants=[RTCObj]} =
        'RTC_ExecutionContextService':get_profile(ECObj),
    'RTC_OK' = 'RTC_ExecutionContextService':remove_component(ECObj, RTCObj),
    #'RTC_ExecutionContextProfile'{participants=[]} =
        'RTC_ExecutionContextService':get_profile(ECObj).


%%-----------------------------------------------------------------------------
%% @doc Tests activating and deactivating a component.
%% @spec act_deact_comp(Config0) ->
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
-spec(act_deact_comp(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
act_deact_comp(Config) ->
    {_RTC, RTC_FSM, RTCObj} = ?config(rtc, Config),
    {_ECSup, _EC_FSM, ECObj} = ?config(ec, Config),
    [EC_FSM2] = rtc:get_owned_contexts(RTC_FSM),
    ECObj2 = ec:get_corba_obj(EC_FSM2),
    'RTC_OK' = 'RTC_ExecutionContextService':activate_component(ECObj2,
        RTCObj),
    'ACTIVE_STATE' = 'RTC_ExecutionContextService':get_component_state(ECObj2,
        RTCObj),
    'RTC_OK' = 'RTC_ExecutionContextService':deactivate_component(ECObj2,
        RTCObj),
    'INACTIVE_STATE' =
        'RTC_ExecutionContextService':get_component_state(ECObj2, RTCObj),
    'RTC_OK' = 'RTC_ExecutionContextService':add_component(ECObj, RTCObj),
    'RTC_OK' = 'RTC_ExecutionContextService':activate_component(ECObj,
        RTCObj),
    'ACTIVE_STATE' = 'RTC_ExecutionContextService':get_component_state(ECObj,
        RTCObj),
    'RTC_OK' = 'RTC_ExecutionContextService':deactivate_component(ECObj,
        RTCObj),
    'INACTIVE_STATE' = 'RTC_ExecutionContextService':get_component_state(ECObj,
        RTCObj),
    'RTC_OK' = 'RTC_ExecutionContextService':remove_component(ECObj, RTCObj).


%%-----------------------------------------------------------------------------
%% @doc Tests resetting a component.
%% @spec reset_comp(Config0) ->
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
-spec(reset_comp(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
reset_comp(Config) ->
    {_RTC, RTC_FSM, RTCObj} = ?config(rtc, Config),
    {_ECSup, _EC_FSM, ECObj} = ?config(ec, Config),
    [EC_FSM2] = rtc:get_owned_contexts(RTC_FSM),
    ECObj2 = ec:get_corba_obj(EC_FSM2),
    'PRECONDITION_NOT_MET' =
        'RTC_ExecutionContextService':reset_component(ECObj2, RTCObj),
    'BAD_PARAMETER' =
        'RTC_ExecutionContextService':reset_component(ECObj, RTCObj),
    'RTC_OK' = 'RTC_ExecutionContextService':add_component(ECObj, RTCObj),
    'PRECONDITION_NOT_MET' =
        'RTC_ExecutionContextService':reset_component(ECObj, RTCObj),
    'RTC_OK' = 'RTC_ExecutionContextService':remove_component(ECObj, RTCObj).

