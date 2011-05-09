%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for SDOPackage_Configuration_impl.erl.
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
-module(sdoc_impl_SUITE).


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
-define(SET1, #'SDOPackage_ConfigurationSet'{id="set1", description="",
        configuration_data=[#'SDOPackage_NameValue'{name="param1",
                value=#any{typecode={tk_string, 0}, value="val1"}},
            #'SDOPackage_NameValue'{name="param2",
                value=#any{typecode={tk_string, 0}, value="val2"}}]}).
-define(SET1_EDITED, #'SDOPackage_ConfigurationSet'{id="set1", description="",
        configuration_data=[#'SDOPackage_NameValue'{name="param1",
                value=#any{typecode={tk_string, 0}, value="val5"}},
        #'SDOPackage_NameValue'{name="param2",
            value=#any{typecode={tk_string, 0}, value="val6"}}]}).
-define(SET2, #'SDOPackage_ConfigurationSet'{id="set2", description="",
        configuration_data=[#'SDOPackage_NameValue'{name="param1",
                value=#any{typecode={tk_string, 0}, value="val3"}},
        #'SDOPackage_NameValue'{name="param2",
            value=#any{typecode={tk_string, 0}, value="val4"}}]}).
-define(SET2_EDITED, #'SDOPackage_ConfigurationSet'{id="set2", description="",
        configuration_data=[#'SDOPackage_NameValue'{name="param1",
                value=#any{typecode={tk_string, 0}, value="val7"}},
        #'SDOPackage_NameValue'{name="param2",
            value=#any{typecode={tk_string, 0}, value="val8"}}]}).
-define(SET3, #'SDOPackage_ConfigurationSet'{id="set3", description="",
        configuration_data=[#'SDOPackage_NameValue'{name="param3",
                value=#any{typecode={tk_string, 0}, value="val5"}},
        #'SDOPackage_NameValue'{name="param5",
            value=#any{typecode={tk_string, 0}, value="val4"}}]}).
-define(BAD_SET, #'SDOPackage_ConfigurationSet'{id="", description="",
        configuration_data=[#'SDOPackage_NameValue'{name="",
                value=#any{typecode={tk_string, 0}, value="val1"}},
            #'SDOPackage_NameValue'{name="param2",
                value=#any{typecode={tk_string, 0}, value=""}}]}).


%%-----------------------------------------------------------------------------
%% Test server callbacks
%%-----------------------------------------------------------------------------
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
        end_per_testcase/2]).


%%-----------------------------------------------------------------------------
%% Test cases
%%-----------------------------------------------------------------------------
-export([get_sets/1, get_set/1, set_values/1, get_set_active/1,
        add_rem_sets/1]).
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
    [get_sets, get_set, set_values, get_set_active, add_rem_sets].


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
    Cfg11 = config:set_value("corba", [{"nameservers", "127.0.0.1:28090"}],
        Cfg10),
    Cfg12 = config:set_value("configuration", [{"active_config", "set1"}],
        Cfg11),
    config:set_value("conf",
        [{"set1", [{"param1", "val1"}, {"param2", "val2"}]},
            {"set2", [{"param1", "val3"}, {"param2", "val4"}]}], Cfg12).


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
init_per_testcase(_TestCase, Config) ->
    {ok, RTC, FSM} = rtc:create(simple_cfg(), fun num_sfx_gen/0,
        mock_rtc_behv),
    {cfg_corba, Cfg_Corba, worker, _} = lists:keyfind(cfg_corba, 1,
        supervisor:which_children(RTC)),
    Obj = corba_obj_mgr:get_obj(Cfg_Corba),
    true = unlink(RTC),
    [{rtc, {RTC, FSM, Obj}} | Config].

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
end_per_testcase(_TestCase, Config) ->
    {RTC, _FSM, _Obj} = ?config(rtc, Config),
    ok = rtc:destroy(RTC).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the configuration sets.
%% @spec get_sets(Config0) ->
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
-spec(get_sets(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_sets(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    Sets = 'SDOPackage_Configuration':get_configuration_sets(Obj),
    true = lists:member(?SET1, Sets),
    true = lists:member(?SET2, Sets).


%%-----------------------------------------------------------------------------
%% @doc Tests getting a specific configuration set.
%% @spec get_set(Config0) ->
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
-spec(get_set(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_set(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    ?SET1 = 'SDOPackage_Configuration':get_configuration_set(Obj, "set1"),
    ?SET2 = 'SDOPackage_Configuration':get_configuration_set(Obj, "set2"),
    {'EXCEPTION', #'SDOPackage_InvalidParameter'{}} = (catch
        'SDOPackage_Configuration':get_configuration_set(Obj, "set3")),
    {'EXCEPTION', #'SDOPackage_InvalidParameter'{}} = (catch
        'SDOPackage_Configuration':get_configuration_set(Obj, "")).


%%-----------------------------------------------------------------------------
%% @doc Tests setting the values of a configuration set.
%% @spec set_values(Config0) ->
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
-spec(set_values(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
set_values(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    true = 'SDOPackage_Configuration':set_configuration_set_values(Obj,
        ?SET1_EDITED),
    ?SET1_EDITED = 'SDOPackage_Configuration':get_configuration_set(Obj,
        "set1"),
    true = 'SDOPackage_Configuration':set_configuration_set_values(Obj,
        ?SET2_EDITED),
    ?SET2_EDITED = 'SDOPackage_Configuration':get_configuration_set(Obj,
        "set2"),
    false = 'SDOPackage_Configuration':set_configuration_set_values(Obj,
        ?SET3),
    {'EXCEPTION', #'SDOPackage_InvalidParameter'{}} = (catch
        'SDOPackage_Configuration':get_configuration_set(Obj, "set3")),
    false =
        'SDOPackage_Configuration':set_configuration_set_values(Obj, ?BAD_SET).


%%-----------------------------------------------------------------------------
%% @doc Tests getting and setting the active configuration set.
%% @spec get_set_active(Config0) ->
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
-spec(get_set_active(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_set_active(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    ?SET1 = 'SDOPackage_Configuration':get_active_configuration_set(Obj),
    true = 'SDOPackage_Configuration':activate_configuration_set(Obj, "set2"),
    false = 'SDOPackage_Configuration':activate_configuration_set(Obj, "set3"),
    false = 'SDOPackage_Configuration':activate_configuration_set(Obj, "_set"),
    ?SET2 = 'SDOPackage_Configuration':get_active_configuration_set(Obj).


%%-----------------------------------------------------------------------------
%% @doc Tests adding and removing configuration sets.
%% @spec add_rem_sets(Config0) ->
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
-spec(add_rem_sets(Config::[tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
add_rem_sets(Config) ->
    {_RTC, _FSM, Obj} = ?config(rtc, Config),
    {'EXCEPTION', #'SDOPackage_InvalidParameter'{}} = (catch
        'SDOPackage_Configuration':get_configuration_set(Obj, "set3")),
    false = 'SDOPackage_Configuration':remove_configuration_set(Obj, "set3"),
    {'EXCEPTION', #'SDOPackage_InvalidParameter'{}} = (catch
        'SDOPackage_Configuration':remove_configuration_set(Obj, "")),
    {'EXCEPTION', #'SDOPackage_InvalidParameter'{}} = (catch
        'SDOPackage_Configuration':add_configuration_set(Obj, ?BAD_SET)),
    false = 'SDOPackage_Configuration':add_configuration_set(Obj, ?SET1),
    true = 'SDOPackage_Configuration':add_configuration_set(Obj, ?SET3),
    ?SET3 = 'SDOPackage_Configuration':get_configuration_set(Obj, "set3"),
    true = 'SDOPackage_Configuration':remove_configuration_set(Obj, "set3"),
    {'EXCEPTION', #'SDOPackage_InvalidParameter'{}} = (catch
        'SDOPackage_Configuration':get_configuration_set(Obj, "set3")).

