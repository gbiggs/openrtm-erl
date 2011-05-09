%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Unit tests for config.erl.
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
-module(config_tests).


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
%% Imports
%%-----------------------------------------------------------------------------
-import(config, [empty_conf/0, get_value/3, get_value/2, set_value/3,
        read_conf_file/1, merge/2, tokens/1, tokens/2, tokens/3, flatten/1,
        unflatten/1, compare/2, compare/4, compare_split/3]).


%%=============================================================================
%% Internal functions
%%=============================================================================


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test creating an empty configuration.
%% @spec empty_conf_test() -> ok
%% @end
-spec(empty_conf_test() -> ok).
%%-----------------------------------------------------------------------------
empty_conf_test() ->
    ?assertMatch([], empty_conf()).


%%-----------------------------------------------------------------------------
%% @doc Test getting values from a configuration.
%% @spec get_value_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(get_value_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
get_value_test_() ->
    [?_assertMatch("value", get_value("param", [{"param", "value"}])),
        ?_assertMatch("value",
            get_value("param", [{"param2", "value2"}, {"param", "value"}])),
        ?_assertMatch("value",
            get_value(["param"], [{"param", "value"}, {"param2", "value2"}])),
        ?_assertMatch("value2",
            get_value(["param", "subparam2"],
                [{"param2", "value3"}, {"param",
                        [{"subparam1", "value1"}, {"subparam2", "value2"}]}])),
        ?_assertMatch(undefined, get_value("param2", [{"param", "value"}])),
        ?_assertMatch(undefined, get_value("param2", [])),
        ?_assertMatch("Default", get_value("param2", "Default",
                [{"param", "value"}])),
        ?_assertMatch("Default", get_value("param2", "Default", []))].


%%-----------------------------------------------------------------------------
%% @doc Test setting values.
%% @spec set_value_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(set_value_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
set_value_test_() ->
    [?_assertMatch([{"param", "value"}], set_value("param", "value", [])),
        ?_assertMatch([{"param", "value2"}],
            set_value("param", "value2", [{"param", "value1"}])),
        ?_assertMatch([{"param1", "value1"}, {"param2", "value2"}],
            set_value("param1", "value1", [{"param2", "value2"}])),
        ?_assertMatch([{"param1", "value3"}, {"param2", "value2"}],
            set_value("param1", "value3",
                [{"param1", "value1"}, {"param2", "value2"}])),
        ?_assertMatch([{"param1", "value1"}, {"param2", [{"param3", true}]}],
            set_value("param2", [{"param3", true}], [{"param1", "value1"}])),
        ?_assertMatch([{"param1", "value1"}, {"param2", [{"param3", true}]}],
            set_value(["param2", "param3"], true, [{"param1", "value1"}])),
        ?_assertMatch([{"param1", "value1"},
                {"param2", [{"param3", true}, {"param4", false}]}],
            set_value(["param2", "param3"], true,
                [{"param1", "value1"}, {"param2", [{"param4", false}]}]))
    ].


%%-----------------------------------------------------------------------------
%% @doc Test reading an empty configuration file.
%% @spec read_empty_cf_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(read_empty_cf_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
read_empty_cf_test_() ->
    {setup,
        fun() ->
            FN = "this_file_is_empty",
            {ok, Fd} = file:open(FN, [write]),
            ok = file:close(Fd),
            {ok, Fd2} = file:open(FN, [read]),
            {FN, Fd2}
        end,
        fun({FN, Fd}) ->
            ok = file:close(Fd),
            ok = file:delete(FN)
        end,
        fun({_, Fd}) ->
            [?_assertMatch({ok, []}, read_conf_file(Fd))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test reading a one-layer configuration file: read test.
%% @spec read_one_layer_cf_read_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(read_one_layer_cf_read_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
read_one_layer_cf_read_test_() ->
    {setup,
        fun() ->
            setup_file("this_file_is_one_layer",
                ["param1: value1", "param2: value2", "param3"])
        end,
        fun(S) -> cleanup_file(S) end,
        fun({_, Fd}) ->
            [?_assertMatch({ok, _}, read_conf_file(Fd))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test reading a one-layer configuration file: accuracy test.
%% @spec read_one_layer_cf_acc_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(read_one_layer_cf_acc_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
read_one_layer_cf_acc_test_() ->
    {setup,
        fun() ->
            setup_config("this_file_is_one_layer",
                ["param1: value1", "param2: value2", "param3"])
        end,
        fun(_) -> ok end,
        fun(C) ->
            [?_assertMatch(["value1"], proplists:get_all_values("param1", C)),
                ?_assertMatch(["value2"],
                    proplists:get_all_values("param2", C)),
                ?_assertMatch(true, proplists:get_bool("param3", C))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test reading a two-layer configuration file: read test.
%% @spec read_two_layer_cf_read_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(read_two_layer_cf_read_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
read_two_layer_cf_read_test_() ->
    {setup,
        fun() ->
            setup_file("this_file_is_two_layers", ["param1: value1",
                    "param2.subparam1: value2", "param2.subparam2"])
        end,
        fun(S) -> cleanup_file(S) end,
        fun({_, Fd}) ->
            [?_assertMatch({ok, _}, read_conf_file(Fd))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test reading a two-layer configuration file: accuracy test.
%% @spec read_two_layer_cf_acc_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(read_two_layer_cf_acc_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
read_two_layer_cf_acc_test_() ->
    {setup,
        fun() ->
            setup_config("this_file_is_two_layers", ["param1: value1",
                    "param2.subparam1: value2", "param2.subparam2"])
        end,
        fun(_) -> ok end,
        fun(C) ->
            [?_assertMatch(["value1"], proplists:get_all_values("param1", C)),
                ?_assertMatch([[{"subparam1", "value2"}, {"subparam2", true}]],
                    proplists:get_all_values("param2", C)),
                ?_assertMatch(["value2"],
                    proplists:get_all_values("subparam1",
                        proplists:get_value("param2", C))),
                ?_assertMatch([true],
                    proplists:get_all_values("subparam2",
                        proplists:get_value("param2", C)))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test reading a configuration file with an error: trailing line
%% continuation.
%% @spec read_bad_cf_line_cont_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(read_bad_cf_line_cont_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
read_bad_cf_line_cont_test_() ->
    {setup,
        fun() ->
            setup_file("this_file_is_bad",
                ["param1: value1", "param2.subparam1: value2\\"])
        end,
        fun(S) -> cleanup_file(S) end,
        fun({_, Fd}) ->
            ?_assertError(function_clause, read_conf_file(Fd))
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test merging configurations.
%% @spec merge_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(merge_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
merge_test_() ->
    [?_assertMatch([{"param", "value"}], merge([], [{"param", "value"}])),
        ?_assertMatch([{"param", "value"}], merge([{"param", "value"}], [])),
        ?_assertMatch([{"param", "value2"}],
            merge([{"param", "value1"}], [{"param", "value2"}])),
        ?_assertMatch([{"param1", "value1"}, {"param2", "value2"}],
            merge([{"param1", "value1"}], [{"param2", "value2"}])),
        ?_assertMatch([{"param1", "value3"}, {"param2", "value2"}],
            merge([{"param1", "value1"}],
                [{"param1", "value3"}, {"param2", "value2"}])),
        ?_assertMatch([{"param1", "value1"}, {"param2", [{"param3", true}]}],
            merge([{"param1", "value1"}], [{"param2", [{"param3", true}]}]))
    ].


%%-----------------------------------------------------------------------------
%% @doc Test tokenising strings.
%% @spec tokens_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(tokens_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
tokens_test_() ->
    [?_assertMatch(["token1"], tokens("token1")),
        ?_assertMatch(["token1"], tokens("token1", ",")),
        ?_assertMatch(["token1"], tokens("token1", ",", both)),
        ?_assertMatch(["token1", "token2"], tokens("token1,token2")),
        ?_assertMatch(["token1", "token2"], tokens("token1,token2", ",")),
        ?_assertMatch(["token1", "token2"], tokens("token1,token2", ",",
                both)),
        ?_assertMatch(["token1,token2"], tokens("token1,token2", " ")),
        ?_assertMatch(["token1,token2"], tokens("token1,token2", " ", both)),
        ?_assertMatch(["token1", "token2"], tokens("token1, token2")),
        ?_assertMatch(["token1", "token2"], tokens("token1, token2", ",")),
        ?_assertMatch(["token1", "token2"], tokens("token1, token2", ",",
                both)),
        ?_assertMatch(["token1", " token2"], tokens("token1, token2", ",",
                right)),
        ?_assertMatch(["token1", "token2"], tokens("token1, token2", ",",
                left)),
        ?_assertMatch([" token1", " token2"], tokens(" token1, token2 ", ",",
                right)),
        ?_assertMatch(["token1", "token2 "], tokens(" token1, token2 ", ",",
                left)),
        ?_assertMatch(["token1"], tokens("token1,", ","))].


%%-----------------------------------------------------------------------------
%% @doc Test flattening a config.
%% @spec flatten_test() -> ok
%% @end
-spec(flatten_test() -> ok).
%%-----------------------------------------------------------------------------
flatten_test() ->
    Flat = flatten([{"1_1", [{"2_1", [{"param1", "val1"}, {"param2", "val2"}]},
                        {"2_2", "val3"},
                        {"2_3", [{"param1", "val4"}]}]},
                {"1_2", [{"2_1", [{"param5", "val5"}]}]},
                {"1_3", "val6"}]),
    ?assertMatch(6, length(Flat)),
    ?assertMatch(true, lists:member({"1_1.2_1.param1", "val1"}, Flat)),
    ?assertMatch(true, lists:member({"1_1.2_1.param2", "val2"}, Flat)),
    ?assertMatch(true, lists:member({"1_1.2_2", "val3"}, Flat)),
    ?assertMatch(true, lists:member({"1_1.2_3.param1", "val4"}, Flat)),
    ?assertMatch(true, lists:member({"1_2.2_1.param5", "val5"}, Flat)),
    ?assertMatch(true, lists:member({"1_3", "val6"}, Flat)).


%%-----------------------------------------------------------------------------
%% @doc Test unflattening a config.
%% @spec unflatten_test() -> ok
%% @end
-spec(unflatten_test() -> ok).
%%-----------------------------------------------------------------------------
unflatten_test() ->
    Flat = [{"1_1.2_1.param1", "val1"},
        {"1_1.2_1.param2", "val2"},
        {"1_1.2_2", "val3"},
        {"1_1.2_3.param1", "val4"},
        {"1_2.2_1.param5", "val5"},
        {"1_3", "val6"}],
    Unflat = unflatten(Flat),
    ?assertMatch(3, length(Unflat)),
    ?assertMatch(3, length(get_value(["1_1"], Unflat))),
    ?assertMatch(2, length(get_value(["1_1", "2_1"], Unflat))),
    ?assertMatch("val1", get_value(["1_1", "2_1", "param1"], Unflat)),
    ?assertMatch("val2", get_value(["1_1", "2_1", "param2"], Unflat)),
    ?assertMatch("val3", get_value(["1_1", "2_2"], Unflat)),
    ?assertMatch("val4", get_value(["1_1", "2_3", "param1"], Unflat)),
    ?assertMatch("val5", get_value(["1_2", "2_1", "param5"], Unflat)),
    ?assertMatch("val6", get_value(["1_3"], Unflat)).


%%-----------------------------------------------------------------------------
%% @doc Test comparing configs.
%% @spec compare_test() -> ok
%% @end
-spec(compare_test() -> ok).
%%-----------------------------------------------------------------------------
compare_test() ->
    C1 = [{"1", [{"2", "3"}, {"4", "5"}]}],
    C2 = [{"1", [{"2", "3"}, {"4", "5"}]}, {"6", "7"}],
    C3 = [{"1", [{"2", "3, 4, 5"}, {"4", "5"}]}],
    C4 = [{"1", [{"2", "6, 7"}, {"4", "5"}]}],
    C5 = [{"1", [{"2", "3, 4, 5"}, {"4", "5"}]}, {"6", "7"}],
    ?assertMatch(true, compare([], [], false, false)),
    ?assertMatch(true, compare([], [], true, false)),
    ?assertMatch(true, compare([], [], false, true)),
    ?assertMatch(true, compare([], [], true, true)),
    ?assertMatch(true, compare([], [])),
    ?assertMatch(true, compare_split([], [], true)),

    ?assertMatch(true, compare(C1, C1, false, false)),
    ?assertMatch(true, compare(C1, C1, true, false)),
    ?assertMatch(true, compare(C1, C1, false, true)),
    ?assertMatch(true, compare(C1, C1, true, true)),
    ?assertMatch(true, compare(C1, C1)),
    ?assertMatch(true, compare_split(C1, C1, true)),

    ?assertMatch(true, compare(C2, C2, false, false)),
    ?assertMatch(true, compare(C2, C2, true, false)),
    ?assertMatch(true, compare(C2, C2, false, true)),
    ?assertMatch(true, compare(C2, C2, true, true)),
    ?assertMatch(true, compare(C2, C2)),
    ?assertMatch(true, compare_split(C2, C2, true)),

    ?assertMatch(false, compare(C1, C2, false, false)),
    ?assertMatch(true, compare(C1, C2, true, false)),
    ?assertMatch(false, compare(C1, C2, false, true)),
    ?assertMatch(true, compare(C1, C2, true, true)),
    ?assertMatch(true, compare(C1, C2)),
    ?assertMatch(true, compare_split(C1, C2, true)),

    ?assertMatch(false, compare(C1, C3, false, false)),
    ?assertMatch(false, compare(C1, C3, true, false)),
    ?assertMatch(true, compare(C1, C3, false, true)),
    ?assertMatch(true, compare(C1, C3, true, true)),
    ?assertMatch(true, compare(C1, C3)),
    ?assertMatch(true, compare_split(C1, C3, true)),

    ?assertMatch(false, compare(C1, C5, false, false)),
    ?assertMatch(false, compare(C1, C5, true, false)),
    ?assertMatch(false, compare(C1, C5, false, true)),
    ?assertMatch(true, compare(C1, C5, true, true)),
    ?assertMatch(true, compare(C1, C5)),
    ?assertMatch(true, compare_split(C1, C5, true)),

    ?assertMatch(false, compare(C3, C4, false, false)),
    ?assertMatch(false, compare(C3, C4, true, false)),
    ?assertMatch(false, compare(C3, C4, false, true)),
    ?assertMatch(false, compare(C3, C4, true, true)),
    ?assertMatch(false, compare(C3, C4)),
    ?assertMatch(false, compare_split(C3, C4, true)).


%%-----------------------------------------------------------------------------
%% @doc Unit test support function that sets up a file with lines of text and
%% reads it into a configuration.
%% @spec setup_config(FN, Lines) -> config()
%% where
%%      FN = string()
%%      Lines = [string()]
%% @end
-spec(setup_config(FN::string(), Lines::[string()]) -> config()).
%%-----------------------------------------------------------------------------
setup_config(FN, Lines) ->
    {ok, Fd} = file:open(FN, [write]),
    lists:foreach(fun(L) -> io:fwrite(Fd, "~s~n", [L]) end, Lines),
    ok = file:close(Fd),
    {ok, Fd2} = file:open(FN, [read]),
    {ok, C} = read_conf_file(Fd2),
    ok = file:close(Fd),
    ok = file:delete(FN),
    C.


%%-----------------------------------------------------------------------------
%% @doc Unit test support function that sets up a file with lines of text.
%% @spec setup_file(FN, Lines) -> State
%% where
%%      FN = string()
%%      Lines = [string()]
%%      State = {FN, Fd}
%%      Fd = pid()
%% @end
-spec(setup_file(FN::string(), Lines::[string()]) ->
    {FN::string(), Fd::pid()}).
%%-----------------------------------------------------------------------------
setup_file(FN, Lines) ->
    {ok, Fd} = file:open(FN, [write]),
    lists:foreach(fun(L) -> io:fwrite(Fd, "~s~n", [L]) end, Lines),
    ok = file:close(Fd),
    {ok, Fd2} = file:open(FN, [read]),
    {FN, Fd2}.


%%-----------------------------------------------------------------------------
%% @doc Unit test support function that cleans up a file.
%% @spec cleanup_file({FN, Fd}) -> ok
%% where
%%      FN = string()
%%      Fd = pid()
%% @end
-spec(cleanup_file({FN::string(), Fd::pid()}) -> ok).
%%-----------------------------------------------------------------------------
cleanup_file({FN, Fd}) ->
    ok = file:close(Fd),
    ok = file:delete(FN).

-endif. % TEST

