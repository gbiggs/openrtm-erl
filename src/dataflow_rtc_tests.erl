%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Unit tests for dataflow_rtc.erl.
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
-module(dataflow_rtc_tests).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("rtc.hrl").
-include("log.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Imports
%%-----------------------------------------------------------------------------
-import(dataflow_rtc, [handle_sync_event/4, created/3, init_wait/3, alive/3,
    finalise_wait/3, exit_wait/3, destroyed/3]).


%%=============================================================================
%% Internal functions
%%=============================================================================


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

% Testing actions:
% - Create a #df_rtc{} with the name of a dummy module
% - The dummy module just sets the state data to the name of the CB called.
% - Call a state function with the #df_rtc{}
% - Check that the state data has changed to the CB that should have been
% called.
% - Check that the result is the correct next state.

%%-----------------------------------------------------------------------------
%% @doc Test setting the configuration server PID.
%% @spec set_cfg_svr_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(set_cfg_svr_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
set_cfg_svr_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, ok, created, #df_rtc{cfg_svr=blurgle}},
                    handle_sync_event({set_cfg_svr, blurgle},
                        none, created, S))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test setting the ports supervisor PID.
%% @spec set_port_mgr_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(set_port_mgr_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
set_port_mgr_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, ok, created, #df_rtc{port_mgr=blurgle}},
                    handle_sync_event({set_port_mgr, blurgle},
                        none, created, S))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test setting the EC supervisor PID.
%% @spec set_ec_sup_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(set_ec_sup_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
set_ec_sup_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, ok, created, #df_rtc{ecs_sup=blurgle}},
                    handle_sync_event({set_ecs_sup, blurgle},
                        none, created, S))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test setting and getting the supervisor PID.
%% @spec sup_pid_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(sup_pid_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
sup_pid_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, ok, created, #df_rtc{sup_pid=blurgle}},
                    handle_sync_event({set_sup, blurgle},
                        none, created, S))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test the created state.
%% @spec created_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(created_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
created_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, false, created, S},
                    created({is_alive, none}, none, S)),
                ?_assertMatch({reply, created, created, S},
                    created(state, none, S)),
                ?_assertMatch({reply, precon_not_met, created, S},
                    created(finalise, none, S)),
                ?_assertMatch({reply, precon_not_met, created, S},
                    created(exit, none, S))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test the init_wait state.
%% @spec init_wait_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(init_wait_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
init_wait_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, false, init_wait, S},
                    init_wait({is_alive, none}, none, S)),
                ?_assertMatch({reply, init_wait, init_wait, S},
                    init_wait(state, none, S)),
                ?_assertMatch({reply, precon_not_met, init_wait, S},
                    init_wait(initialise, none, S)),
                ?_assertMatch({reply, precon_not_met, init_wait, S},
                    init_wait(finalise, none, S)),
                ?_assertMatch({reply, precon_not_met, init_wait, S},
                    init_wait(exit, none, S))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test the alive state.
%% @spec alive_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(alive_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
alive_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, false, alive, S},
                    alive({is_alive, none}, none, S)),
                ?_assertMatch({reply, alive, alive, S},
                    alive(state, none, S)),
                ?_assertMatch({reply, precon_not_met, alive, S},
                    alive(initialise, none, S))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test the finalise_wait state.
%% @spec finalise_wait_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(finalise_wait_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
finalise_wait_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, false, finalise_wait, S},
                    finalise_wait({is_alive, none}, none, S)),
                ?_assertMatch({reply, finalise_wait, finalise_wait, S},
                    finalise_wait(state, none, S)),
                ?_assertMatch({reply, precon_not_met, finalise_wait, S},
                    finalise_wait(initialise, none, S)),
                ?_assertMatch({reply, precon_not_met, finalise_wait, S},
                    finalise_wait(finalise, none, S)),
                ?_assertMatch({reply, precon_not_met, finalise_wait, S},
                    finalise_wait(exit, none, S))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test the exit_wait state.
%% @spec exit_wait_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(exit_wait_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
exit_wait_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, false, exit_wait, S},
                    exit_wait({is_alive, none}, none, S)),
                ?_assertMatch({reply, exit_wait, exit_wait, S},
                    exit_wait(state, none, S)),
                ?_assertMatch({reply, precon_not_met, exit_wait, S},
                    exit_wait(initialise, none, S)),
                ?_assertMatch({reply, precon_not_met, exit_wait, S},
                    exit_wait(finalise, none, S)),
                ?_assertMatch({reply, precon_not_met, exit_wait, S},
                    exit_wait(exit, none, S))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test the destroyed state.
%% @spec destroyed_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(destroyed_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
destroyed_test_() ->
    {setup,
        fun() ->
            #df_rtc{mod=none}
        end,
        fun(_S) ->
            ok
        end,
        fun(S) ->
            [?_assertMatch({reply, false, destroyed, S},
                    destroyed({is_alive, none}, none, S)),
                ?_assertMatch({reply, destroyed, destroyed, S},
                    destroyed(state, none, S)),
                ?_assertMatch({reply, precon_not_met, destroyed, S},
                    destroyed(initialise, none, S)),
                ?_assertMatch({reply, precon_not_met, destroyed, S},
                    destroyed(finalise, none, S)),
                ?_assertMatch({reply, precon_not_met, destroyed, S},
                    destroyed(exit, none, S))]
        end
    }.

-endif. % TEST

