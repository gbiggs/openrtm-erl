%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Configuration file server tests.
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
-module(config_svr_tests).


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
-import(config_svr, [init/1, handle_call/3]).


%%=============================================================================
%% Internal functions
%%=============================================================================


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with a file that doesn't exist.
%% @spec init_no_conf_file_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(init_no_conf_file_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
init_no_conf_file_test_() ->
    [?_assertMatch({ok, []}, init({"this_file_does_not_exist", []})),
        ?_assertMatch({ok, [{"param", "value"}]},
            init({"this_file_does_not_exist", [{"param", "value"}]}))].


%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with a file that does exist.
%% @spec init_with_conf_file_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(init_with_conf_file_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
init_with_conf_file_test_() ->
    {setup,
        fun() ->
            FN = "this_file_does_exist",
            {ok, Fd} = file:open(FN, [write]),
            io:fwrite(Fd, "param1: value1~nparam2: value2~nparam3", []),
            ok = file:close(Fd),
            FN
        end,
        fun(FN) -> ok = file:delete(FN) end,
        fun(FN) ->
            [?_assertMatch({ok, [{"param1", "value1"}, {"param2", "value2"},
                            {"param3", true}]},
                    init({FN, []}))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with a file for which permission doesn't exist.
%% @spec init_no_permissions_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(init_no_permissions_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
init_no_permissions_test_() ->
    [?_assertError({case_clause, {error, eacces}}, init({"/root/file", []}))].


%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with a file that overwrites a default.
%% @spec init_overwrite_test_() ->  [{integer(), fun(() -> ok)}]
%% @end
-spec(init_overwrite_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
init_overwrite_test_() ->
    {setup,
        fun() ->
            FN = "this_file_does_exist",
            {ok, Fd} = file:open(FN, [write]),
            io:fwrite(Fd, "param: value2~n", []),
            ok = file:close(Fd),
            FN
        end,
        fun(FN) -> ok = file:delete(FN) end,
        fun(FN) ->
            [?_assertMatch({ok, [{"param", "value2"}]},
                        init({FN, [{"param", "value1"}]}))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with a file that does not overwrite a default.
%% @spec init_merge_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(init_merge_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
init_merge_test_() ->
    {setup,
        fun() ->
            FN = "this_file_does_exist",
            {ok, Fd} = file:open(FN, [write]),
            io:fwrite(Fd, "param2: value2", []),
            ok = file:close(Fd),
            FN
        end,
        fun(FN) -> ok = file:delete(FN) end,
        fun(FN) ->
            [?_assertMatch({ok, [{"param1", "value1"}, {"param2", "value2"}]},
                    init({FN, [{"param1", "value1"}]}))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Tests getting a value from a server.
%% @spec get_value_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(get_value_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
get_value_test_() ->
    [?_assertMatch({reply, "value", [{"param", "value"}]},
            handle_call({get_value, "param"}, {self(), tag},
                [{"param", "value"}]))].

-endif. % TEST

