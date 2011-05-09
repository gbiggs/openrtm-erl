%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Unit tests for config_set.erl.
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
-module(config_set_tests).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("config_set.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Imports
%%-----------------------------------------------------------------------------
-import(config_set, [init/0, init/1, get_sets/1, get_active_set/1,
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
%% @doc Test creating empty config sets.
%% @spec empty_sets_test() -> ok
%% @end
-spec(empty_sets_test() -> ok).
%%-----------------------------------------------------------------------------
empty_sets_test() ->
    ?assertMatch(#cfg_sets{sets=[{"default", []}], act=[], act_name="default"},
        init()),
    ?assertMatch(#cfg_sets{sets=[{"default", []}], act=[], act_name="default"},
        init([])).


%%-----------------------------------------------------------------------------
%% @doc Test creating configuration sets from a config.
%% @spec populated_sets_test() -> ok
%% @end
-spec(populated_sets_test() -> ok).
%%-----------------------------------------------------------------------------
populated_sets_test() ->
    ?assertEqual(test_cfg_sets("set1"), init(test_config("set1"))),
    ?assertEqual(test_cfg_sets("set2"), init(test_config("set2"))),
    ?assertEqual(test_cfg_sets_default("set2"),
        init(test_config_default("set2"))),
    ?assertEqual(test_cfg_sets_default("default"),
        init(test_config_default("default"))),
    ?assertMatch(["set1"], get_sets(init(test_config_bad_set()))).


%%-----------------------------------------------------------------------------
%% @doc Test creating with an invalid active set name.
%% @spec invalid_active_test() -> ok
%% @end
-spec(invalid_active_test() -> ok).
%%-----------------------------------------------------------------------------
invalid_active_test() ->
    ?assertEqual(test_cfg_sets("set1"), init(test_config("blurgle"))),
    ?assertEqual(test_cfg_sets_default("default"),
        init(test_config_default("blurgle"))).


%%-----------------------------------------------------------------------------
%% @doc Test get_sets.
%% @spec get_sets_test() -> ok
%% @end
-spec(get_sets_test() -> ok).
%%-----------------------------------------------------------------------------
get_sets_test() ->
    ?assertMatch(["set1", "set2"], get_sets(test_cfg_sets("set1"))),
    ?assertMatch(["default", "set1", "set2"],
        get_sets(test_cfg_sets_default("set1"))).


%%-----------------------------------------------------------------------------
%% @doc Test set_active_set and get_active_set.
%% @spec active_set_name_test() -> ok
%% @end
-spec(active_set_name_test() -> ok).
%%-----------------------------------------------------------------------------
active_set_name_test() ->
    ?assertMatch("set1", get_active_set(test_cfg_sets("set1"))),
    ?LET({ok, NewCSets}, set_active_set("set2", test_cfg_sets("set1")),
        ?assertMatch("set2", get_active_set(NewCSets))),
    ?assertMatch({{error, no_set}, _},
        set_active_set("set3", test_cfg_sets("set1"))).


%%-----------------------------------------------------------------------------
%% @doc Test rem_set.
%% @spec rem_set_test() -> ok
%% @end
-spec(rem_set_test() -> ok).
%%-----------------------------------------------------------------------------
rem_set_test() ->
    ?LET({ok, NewCSets}, rem_set("set2", test_cfg_sets("set1")),
        ?assertMatch({error, no_set}, get_values("set2", NewCSets))),
    ?assertMatch({{error, active_set}, _}, rem_set("set1",
            test_cfg_sets("set1"))),
    ?assertMatch({{error, no_set}, _}, rem_set("set3", test_cfg_sets("set1"))).


%%-----------------------------------------------------------------------------
%% @doc Test get_values.
%% @spec get_values_test() -> ok
%% @end
-spec(get_values_test() -> ok).
%%-----------------------------------------------------------------------------
get_values_test() ->
    ?assertMatch([{"param1", "val1"}, {"param2", "val2"}],
        get_values("set1", test_cfg_sets("set1"))),
    ?assertMatch([{"param1", "val3"}, {"param2", "val4"}],
        get_values("set2", test_cfg_sets("set1"))).


%%-----------------------------------------------------------------------------
%% @doc Test set_values.
%% @spec set_values_test() -> ok
%% @end
-spec(set_values_test() -> ok).
%%-----------------------------------------------------------------------------
set_values_test() ->
    ?LET(NewCSets, set_values("set1",
            [{"param1", "val5"}, {"param2", "val6"}], test_cfg_sets("set1")),
        ?assertMatch([{"param1", "val5"}, {"param2", "val6"}],
            get_values("set1", NewCSets))),
    ?LET(NewCSets, set_values("set1",
            [{"param1", "val5"}, {"param2", "val6"}], test_cfg_sets("set1")),
        ?assertMatch("val5", get_value("param1", NewCSets))),
    ?LET(NewCSets, set_values("set3",
            [{"param3", "val5"}, {"param4", "val6"}], test_cfg_sets("set1")),
        ?assertMatch([{"param3", "val5"}, {"param4", "val6"}],
            get_values("set3", NewCSets))),
    ?LET(NewCSets, set_values("set3",
            [{"param3", "val5"}, {"param4", "val6"}], test_cfg_sets("set1")),
        ?assertMatch("val1", get_value("param1", NewCSets))).


%%-----------------------------------------------------------------------------
%% @doc Test get_value.
%% @spec get_value_test() -> ok
%% @end
-spec(get_value_test() -> ok).
%%-----------------------------------------------------------------------------
get_value_test() ->
    ?assertMatch("val1", get_value("param1", test_cfg_sets("set1"))),
    ?assertMatch("val2", get_value("param2", test_cfg_sets("set1"))),
    ?assertMatch("val3", get_value("param1", test_cfg_sets("set2"))),
    ?assertMatch("val4", get_value("param2", test_cfg_sets("set2"))),
    ?assertMatch({error, no_param},
        get_value("param3", test_cfg_sets("set1"))),
    ?assertMatch("val3", get_value("set2", "param1", test_cfg_sets("set1"))),
    ?assertMatch("val4", get_value("set2", "param2", test_cfg_sets("set1"))),
    ?assertMatch("val1", get_value("set1", "param1", test_cfg_sets("set2"))),
    ?assertMatch("val2", get_value("set1", "param2", test_cfg_sets("set2"))),
    ?assertMatch("val1", get_value("set1", "param1", test_cfg_sets("set1"))),
    ?assertMatch("val2", get_value("set1", "param2", test_cfg_sets("set1"))),
    ?assertMatch("val3", get_value("set2", "param1", test_cfg_sets("set2"))),
    ?assertMatch("val4", get_value("set2", "param2", test_cfg_sets("set2"))).


%%-----------------------------------------------------------------------------
%% @doc Test set_value.
%% @spec set_value_test() -> ok
%% @end
-spec(set_value_test() -> ok).
%%-----------------------------------------------------------------------------
set_value_test() ->
    ?LET({ok, NewCSets}, set_value("param1", "val5", test_cfg_sets("set1")),
        ?assertMatch("val5", get_value("param1", NewCSets))),
    ?LET({ok, NewCSets},
        set_value("set1", "param1", "val5", test_cfg_sets("set1")),
        ?assertMatch("val5", get_value("param1", NewCSets))),
    ?LET({ok, NewCSets},
        set_value("set2", "param1", "val5", test_cfg_sets("set1")),
        ?assertMatch("val5",
            get_value("set2", "param1", NewCSets))),
    ?LET({ok, NewCSets},
        set_value("set2", "param3", "val5", test_cfg_sets("set1")),
        ?assertMatch("val5",
            get_value("set2", "param3", NewCSets))),
    ?assertMatch({{error, no_set}, #cfg_sets{}},
        set_value("set3", "param1", "val5", test_cfg_sets("set1"))).


%%-----------------------------------------------------------------------------
%% @doc Test has_set.
%% @spec has_set_test() -> ok
%% @end
-spec(has_set_test() -> ok).
%%-----------------------------------------------------------------------------
has_set_test() ->
    ?assertMatch(true, has_set("set1", test_cfg_sets("set1"))),
    ?assertMatch(true, has_set("set2", test_cfg_sets("set1"))),
    ?assertMatch(false, has_set("set3", test_cfg_sets("set1"))).


%%-----------------------------------------------------------------------------
%% @doc Test has_key.
%% @spec has_key_test() -> ok
%% @end
-spec(has_key_test() -> ok).
%%-----------------------------------------------------------------------------
has_key_test() ->
    ?assertMatch(true, has_key("set1", "param1", test_cfg_sets("set1"))),
    ?assertMatch(true, has_key("set1", "param2", test_cfg_sets("set1"))),
    ?assertMatch(false, has_key("set1", "param3", test_cfg_sets("set1"))),
    ?assertMatch(true, has_key("set2", "param1", test_cfg_sets("set1"))),
    ?assertMatch(true, has_key("set2", "param2", test_cfg_sets("set1"))),
    ?assertMatch(false, has_key("set2", "param3", test_cfg_sets("set1"))),
    ?assertMatch(false, has_key("set3", "param1", test_cfg_sets("set1"))).


%%-----------------------------------------------------------------------------
%% @doc Supply a sample config for testing with.
%% @spec test_config(string()) -> config()
%% @end
-spec(test_config(ActiveSet::string()) -> config()).
%%-----------------------------------------------------------------------------
test_config(ActiveSet) ->
    [{"configuration", [{"active_config", ActiveSet}]},
        {"conf", [{"set1", [{"param1", "val1"}, {"param2", "val2"}]},
                {"set2", [{"param1", "val3"}, {"param2", "val4"}]}
            ]
        }
    ].


%%-----------------------------------------------------------------------------
%% @doc Supply a sample config with a default set for testing with.
%% @spec test_config_default(string()) -> config()
%% @end
-spec(test_config_default(ActiveSet::string()) -> config()).
%%-----------------------------------------------------------------------------
test_config_default(ActiveSet) ->
    [{"configuration", [{"active_config", ActiveSet}]},
        {"conf", [{"default", [{"param1", "def1"}, {"param2", "def2"}]},
                {"set1", [{"param1", "val1"}, {"param2", "val2"}]},
                {"set2", [{"param1", "val3"}, {"param2", "val4"}]}
            ]
        }
    ].


%%-----------------------------------------------------------------------------
%% @doc Supply a sample config with a bad set for testing with.
%% @spec test_config_bad_set() -> config()
%% @end
-spec(test_config_bad_set() -> config()).
%%-----------------------------------------------------------------------------
test_config_bad_set() ->
    [{"configuration", [{"active_config", "set1"}]},
        {"conf", [{"set1", [{"param1", "val1"}, {"param2", "val2"}]},
                {"set2", "val3"}
            ]
        }
    ].


%%-----------------------------------------------------------------------------
%% @doc The configuration set that should be built from the sample config.
%% @spec test_cfg_sets(string()) -> #cfg_sets{}
%% @end
-spec(test_cfg_sets(ActiveSet::string()) -> #cfg_sets{}).
%%-----------------------------------------------------------------------------
test_cfg_sets("set1") ->
    #cfg_sets{sets=[{"set1", [{"param1", "val1"}, {"param2", "val2"}]},
            {"set2", [{"param1", "val3"}, {"param2", "val4"}]}],
        act=[{"param1", "val1"}, {"param2", "val2"}], act_name="set1"};
test_cfg_sets("set2") ->
    #cfg_sets{sets=[{"set1", [{"param1", "val1"}, {"param2", "val2"}]},
            {"set2", [{"param1", "val3"}, {"param2", "val4"}]}],
        act=[{"param1", "val3"}, {"param2", "val4"}], act_name="set2"};
test_cfg_sets(_) ->
    #cfg_sets{sets=[{"set1", [{"param1", "val1"}, {"param2", "val2"}]},
            {"set2", [{"param1", "val3"}, {"param2", "val4"}]}],
        act=[{"param1", "val1"}, {"param2", "val2"}], act_name="set1"}.

%%-----------------------------------------------------------------------------
%% @doc The configuration set that should be built from the sample config with
%% defaults.
%% @spec test_cfg_sets_default(string()) -> #cfg_sets{}
%% @end
-spec(test_cfg_sets_default(ActiveSet::string()) -> #cfg_sets{}).
%%-----------------------------------------------------------------------------
test_cfg_sets_default("default") ->
    #cfg_sets{sets=[{"default", [{"param1", "def1"}, {"param2", "def2"}]},
            {"set1", [{"param1", "val1"}, {"param2", "val2"}]},
            {"set2", [{"param1", "val3"}, {"param2", "val4"}]}],
        act=[{"param1", "def1"}, {"param2", "def2"}], act_name="default"};
test_cfg_sets_default("set1") ->
    #cfg_sets{sets=[{"default", [{"param1", "def1"}, {"param2", "def2"}]},
            {"set1", [{"param1", "val1"}, {"param2", "val2"}]},
            {"set2", [{"param1", "val3"}, {"param2", "val4"}]}],
        act=[{"param1", "val1"}, {"param2", "val2"}], act_name="set1"};
test_cfg_sets_default("set2") ->
    #cfg_sets{sets=[{"default", [{"param1", "def1"}, {"param2", "def2"}]},
            {"set1", [{"param1", "val1"}, {"param2", "val2"}]},
            {"set2", [{"param1", "val3"}, {"param2", "val4"}]}],
        act=[{"param1", "val3"}, {"param2", "val4"}], act_name="set2"};
test_cfg_sets_default(_) ->
    #cfg_sets{sets=[{"default", [{"param1", "def1"}, {"param2", "def2"}]},
            {"set1", [{"param1", "val1"}, {"param2", "val2"}]},
            {"set2", [{"param1", "val3"}, {"param2", "val4"}]}],
        act=[{"param1", "def1"}, {"param2", "def2"}], act_name="default"}.

-endif. % TEST

