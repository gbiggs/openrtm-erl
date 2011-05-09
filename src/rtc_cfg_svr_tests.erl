%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Unit tests for rtc_cfg_svr.erl.
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
-module(rtc_cfg_svr_tests).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("config_set.hrl").
-include("log.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Imports
%%-----------------------------------------------------------------------------
-import(rtc_cfg_svr, [start_link/1, stop/1, get_sets/1, get_active_set/1,
        set_active_set/2, rem_set/2, get_values/2, set_values/3, get_value/2,
        get_value/3, set_value/3, set_value/4, has_set/2, has_key/3]).


%%=============================================================================
%% Internal functions
%%=============================================================================


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test starting and stopping the server.
%% @spec start_stop_test_() -> ok
%% @end
-spec(start_stop_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
start_stop_test_() ->
    {setup,
        fun() ->
            ok
        end,
        fun(_) ->
            ok
        end,
        fun(_) ->
            ?LET({ok, Pid}, start_link(test_config()),
                [?_assertMatch(true, is_process_alive(Pid)),
                    ?_assertMatch(ok, stop(Pid)),
                    ?_test(timer:sleep(100)),
                    ?_assertMatch(false, is_process_alive(Pid))])
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test get_sets.
%% @spec get_sets_test_() -> ok
%% @end
-spec(get_sets_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_sets_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(test_config()),
            Pid
        end,
        fun(Pid) ->
            stop(Pid)
        end,
        fun(Pid) ->
            ?_assertMatch(["set1", "set2"], get_sets(Pid))
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test set_active_set and get_active_set.
%% @spec active_set_name_test_() -> ok
%% @end
-spec(active_set_name_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
active_set_name_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(test_config()),
            Pid
        end,
        fun(Pid) ->
            stop(Pid)
        end,
        fun(Pid) ->
            [?_assertMatch("set1", get_active_set(Pid)),
                ?_assertMatch(ok, set_active_set(Pid, "set2")),
                ?_assertMatch("set2", get_active_set(Pid)),
                ?_assertMatch({error, no_set}, set_active_set(Pid, "set3"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test rem_set
%% @spec rem_set_test_() -> ok
%% @end
-spec(rem_set_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
rem_set_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(test_config()),
            Pid
        end,
        fun(Pid) ->
            stop(Pid)
        end,
        fun(Pid) ->
            [?_assertMatch({error, active_set}, rem_set(Pid, "set1")),
                ?_assertMatch(ok, rem_set(Pid, "set2")),
                ?_assertMatch({error, no_set}, rem_set(Pid, "set3"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test get_values
%% @spec get_values_test_() -> ok
%% @end
-spec(get_values_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_values_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(test_config()),
            Pid
        end,
        fun(Pid) ->
            stop(Pid)
        end,
        fun(Pid) ->
            [?_assertMatch([{"param1", "val1"}, {"param2", "val2"}],
                    get_values(Pid, "set1")),
                ?_assertMatch([{"param1", "val3"}, {"param2", "val4"}],
                    get_values(Pid, "set2"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test get_value
%% @spec get_value_test_() -> ok
%% @end
-spec(get_value_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_value_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(test_config()),
            Pid
        end,
        fun(Pid) ->
            stop(Pid)
        end,
        fun(Pid) ->
            [?_assertMatch("val1", get_value(Pid, "param1")),
                ?_assertMatch("val2", get_value(Pid, "param2")),
                ?_assertMatch({error, no_param}, get_value(Pid, "param3")),
                ?_assertMatch("val3", get_value(Pid, "set2", "param1")),
                ?_assertMatch("val4", get_value(Pid, "set2", "param2")),
                ?_assertMatch("val1", get_value(Pid, "set1", "param1")),
                ?_assertMatch("val2", get_value(Pid, "set1", "param2")),
                ?_assertMatch("val1", get_value(Pid, "set1", "param1")),
                ?_assertMatch("val2", get_value(Pid, "set1", "param2")),
                ?_assertMatch("val3", get_value(Pid, "set2", "param1")),
                ?_assertMatch("val4", get_value(Pid, "set2", "param2"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test set_value.
%% @spec set_value_test_() -> ok
%% @end
-spec(set_value_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
set_value_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(test_config()),
            Pid
        end,
        fun(Pid) ->
            stop(Pid)
        end,
        fun(Pid) ->
            [?_assertMatch(ok, set_value(Pid, "param1", "val5")),
                ?_assertMatch("val5", get_value(Pid, "param1")),
                ?_assertMatch(ok, set_value(Pid, "set1", "param1", "val6")),
                ?_assertMatch("val6", get_value(Pid, "param1")),
                ?_assertMatch(ok, set_value(Pid, "set2", "param1", "val7")),
                ?_assertMatch("val7", get_value(Pid, "set2", "param1")),
                ?_assertMatch(ok, set_value(Pid, "set2", "param3", "val8")),
                ?_assertMatch("val8", get_value(Pid, "set2", "param3")),
                ?_assertMatch({error, no_set},
                    set_value(Pid, "set3", "p", "v"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test set_values.
%% @spec set_values_test_() -> ok
%% @end
-spec(set_values_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
set_values_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(test_config()),
            Pid
        end,
        fun(Pid) ->
            stop(Pid)
        end,
        fun(Pid) ->
            [?_assertMatch(ok, set_values(Pid, "set1",
                    [{"param1", "val5"}, {"param2", "val6"}])),
                ?_assertMatch([{"param1", "val5"}, {"param2", "val6"}],
                    get_values(Pid, "set1"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test has_set.
%% @spec has_set_test_() -> ok
%% @end
-spec(has_set_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
has_set_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(test_config()),
            Pid
        end,
        fun(Pid) ->
            stop(Pid)
        end,
        fun(Pid) ->
            [?_assertMatch(true, has_set(Pid, "set1")),
                ?_assertMatch(true, has_set(Pid, "set2")),
                ?_assertMatch(false, has_set(Pid, "set3"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test has_key.
%% @spec has_key_test_() -> ok
%% @end
-spec(has_key_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
has_key_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(test_config()),
            Pid
        end,
        fun(Pid) ->
            stop(Pid)
        end,
        fun(Pid) ->
            [?_assertMatch(true, has_key(Pid, "set1", "param1")),
                ?_assertMatch(true, has_key(Pid, "set2", "param2")),
                ?_assertMatch(false, has_key(Pid, "set2", "param3")),
                ?_assertMatch(false, has_key(Pid, "set3", "param1")),
                ?_assertMatch(false, has_key(Pid, "set3", "param4"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Supply a sample config for testing with.
%% @spec test_config() -> config()
%% @end
-spec(test_config() -> config()).
%%-----------------------------------------------------------------------------
test_config() ->
    [{"configuration", [{"active_config", "set1"}]},
        {"conf", [{"set1", [{"param1", "val1"}, {"param2", "val2"}]},
                {"set2", [{"param1", "val3"}, {"param2", "val4"}]}
            ]
        }
    ].

-endif. % TEST

