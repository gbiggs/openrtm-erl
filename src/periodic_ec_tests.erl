%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Unit tests for periodic_ec.erl.
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
-module(periodic_ec_tests).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("ec.hrl").
-include("log.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Imports
%%-----------------------------------------------------------------------------
-import(periodic_ec, [start_link_fsm/1, start_fsm/1, stop_fsm/1]).
-import(ec, [is_running/1, start/1, stop/1, get_rate/1, set_rate/2,
        add_component/2, remove_component/2, activate_component/2,
        deactivate_component/2, reset_component/2, get_component_state/2,
        get_kind/1, get_profile/1, get_owner/1, get_parts/1, set_owner/2]).


%%=============================================================================
%% Internal functions
%%=============================================================================


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test creating and destroying an execution context.
%% @spec create_and_destroy_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(create_and_destroy_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
create_and_destroy_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "create_and_destroy_test_()"),
            {ok, EC} = start_fsm([{"rate", "10.0"}]),
            EC
        end,
        fun(_) ->
            ok
        end,
        fun(EC) ->
            [?_assertMatch(true, is_process_alive(EC)),
                ?_assertMatch(ok, stop_fsm(EC)),
                ?_test(timer:sleep(100)),
                ?_assertMatch(false, is_process_alive(EC))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test creating and destroying an execution context with a participant.
%% @spec create_and_destroy_with_part_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(create_and_destroy_with_part_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
create_and_destroy_with_part_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "create_and_destroy_with_part_test_()"),
            {ok, EC} = start_fsm([{"rate", "10.0"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = add_component(EC, RTC),
            ok = activate_component(EC, RTC),
            {_, ECP, RTC} = lists:keyfind(RTC, 3, ec:get_parts(EC)),
            mock_rtc:cbs(RTC),
            {EC, RTC, ECP}
        end,
        fun({_EC, RTC, _ECP}) ->
            ok = mock_rtc:stop_fsm(RTC)
        end,
        fun({EC, RTC, ECP}) ->
            [?_assertMatch(true, is_process_alive(EC)),
                ?_assertMatch(ok, stop_fsm(EC)),
                ?_test(timer:sleep(100)),
                ?_assertMatch(false, is_process_alive(EC)),
                ?_assertMatch(false, is_process_alive(ECP)),
                ?_assertMatch([detach_context, on_deactivated],
                    mock_rtc:cbs(RTC))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test changing the state of an EC with no participants.
%% @spec start_and_stop_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(start_and_stop_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
start_and_stop_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "start_and_stop_test_()"),
            {ok, EC} = start_fsm([{"rate", "10.0"}]),
            EC
        end,
        fun(EC) ->
            stop_fsm(EC)
        end,
        fun(EC) ->
            [?_assertMatch(ok, start(EC)),
                ?_test(timer:sleep(1000)),
                ?_assertMatch(true, is_running(EC)),
                ?_assertMatch(ok, stop(EC)),
                ?_test(timer:sleep(100)),
                ?_assertMatch(false, is_running(EC))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test setting the owner of an EC.
%% @spec set_owner_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(set_owner_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
set_owner_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "set_owner_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {EC, RTC}
        end,
        fun({EC, RTC}) ->
            ok = mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, RTC}) ->
            [?_assertMatch(ok, set_owner(EC, RTC)),
                ?_assertMatch(RTC, get_owner(EC)),
                ?_assertMatch([], get_parts(EC))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test adding a new participating component.
%% @spec add_component_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(add_component_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
add_component_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "add_component_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            {ok, RTC1} = mock_rtc:start_link_fsm(dataflow),
            {ok, RTC2} = mock_rtc:start_link_fsm(fsm),
            {EC, RTC1, RTC2}
        end,
        fun({EC, RTC1, RTC2}) ->
            ok = remove_component(EC, RTC1),
            ok = mock_rtc:stop_fsm(RTC1),
            ok = mock_rtc:stop_fsm(RTC2),
            stop_fsm(EC)
        end,
        fun({EC, RTC1, RTC2}) ->
            [?_assertMatch(ok, add_component(EC, RTC1)),
                ?_assertMatch(true,
                    lists:keymember(RTC1, 3, ec:get_parts(EC))),
                ?_assertMatch(inactive, ec:get_component_state(EC, RTC1)),
                ?_assertMatch([attach_context], mock_rtc:cbs(RTC1)),
                ?_assertMatch(precon_not_met, add_component(EC, RTC2))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test removing a participating component.
%% @spec remove_component_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(remove_component_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
remove_component_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "remove_component_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = add_component(EC, RTC),
            {_, ECP, RTC} = lists:keyfind(RTC, 3, ec:get_parts(EC)),
            {EC, RTC, ECP}
        end,
        fun({EC, RTC, _ECP}) ->
            ok = mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, RTC, ECP}) ->
            [?_assertMatch(ok, remove_component(EC, RTC)),
                ?_assertMatch(false, lists:keymember(RTC, 3,
                        ec:get_parts(EC))),
                ?_test(timer:sleep(100)),
                ?_assertMatch(false, is_process_alive(ECP)),
                ?_assertMatch(bad_param, remove_component(EC, self))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test activating a participating component.
%% @spec activate_component_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(activate_component_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
activate_component_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "activate_component_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = add_component(EC, RTC),
            {_, ECP, RTC} = lists:keyfind(RTC, 3, ec:get_parts(EC)),
            mock_rtc:cbs(RTC),
            {EC, RTC, ECP}
        end,
        fun({EC, RTC, _ECP}) ->
            deactivate_component(EC, RTC),
            ok = remove_component(EC, RTC),
            ok = mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, RTC, ECP}) ->
            [?_assertMatch(ok, activate_component(EC, RTC)),
                ?_assertMatch(active, ec:get_component_state(EC, RTC)),
                ?_assertMatch(active, ec_part:state(ECP)),
                ?_assertMatch([on_activated], mock_rtc:cbs(RTC)),
                ?_assertMatch(precon_not_met, activate_component(EC, RTC)),
                ?_assertMatch(bad_param, activate_component(EC, self()))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test deactivating a participating component.
%% @spec deactivate_component_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(deactivate_component_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
deactivate_component_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "deactivate_component_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = add_component(EC, RTC),
            {_, ECP, RTC} = lists:keyfind(RTC, 3, ec:get_parts(EC)),
            ok = activate_component(EC, RTC),
            mock_rtc:cbs(RTC),
            {EC, RTC, ECP}
        end,
        fun({EC, RTC, _ECP}) ->
            ok = remove_component(EC, RTC),
            ok = mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, RTC, ECP}) ->
            [?_assertMatch(ok, deactivate_component(EC, RTC)),
                ?_assertMatch(inactive, ec:get_component_state(EC, RTC)),
                ?_assertMatch(inactive, ec_part:state(ECP)),
                ?_assertMatch([on_deactivated], mock_rtc:cbs(RTC)),
                ?_assertMatch(precon_not_met, deactivate_component(EC, RTC)),
                ?_assertMatch(bad_param, deactivate_component(EC, self()))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test resetting a participating component.
%% @spec reset_component_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(reset_component_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
reset_component_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "reset_component_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = add_component(EC, RTC),
            mock_rtc:next_event_to_error(RTC),
            error = activate_component(EC, RTC),
            mock_rtc:cbs(RTC),
            {EC, RTC}
        end,
        fun({EC, RTC}) ->
            ok = remove_component(EC, RTC),
            ok = mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, RTC}) ->
            [?_assertMatch(ok, reset_component(EC, RTC)),
                ?_assertMatch(inactive, ec:get_component_state(EC, RTC)),
                ?_assertMatch([on_reset], mock_rtc:cbs(RTC)),
                ?_assertMatch(precon_not_met, reset_component(EC, RTC)),
                ?_assertMatch(bad_param, reset_component(EC, self()))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test getting the life cycle state of a participating component.
%% @spec get_component_state_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_component_state_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_component_state_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "get_component_state_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = add_component(EC, RTC),
            {_, ECP, RTC} = lists:keyfind(RTC, 3, ec:get_parts(EC)),
            {EC, ECP, RTC}
        end,
        fun({EC, _ECP, RTC}) ->
            ok = remove_component(EC, RTC),
            mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, ECP, RTC}) ->
            [?_assertMatch(inactive, get_component_state(EC, RTC)),
                ?_assertMatch(ok, activate_component(EC, RTC)),
                ?_assertMatch(active, get_component_state(EC, RTC)),
                ?_test(mock_rtc:next_event_to_error(RTC)),
                ?_assertMatch(error, ec_part:tick(ECP)),
                ?_assertMatch(error, get_component_state(EC, RTC)),
                ?_assertMatch(ok, reset_component(EC, RTC)),
                ?_assertMatch(inactive, get_component_state(EC, RTC))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test getting and setting the rate of a stopped EC.
%% @spec get_and_set_rate_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_and_set_rate_stopped_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_and_set_rate_stopped_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "get_and_set_rate_stopped_test_()"),
            {ok, EC} = start_fsm([{"rate", "10.0"}]),
            EC
        end,
        fun(EC) ->
            stop_fsm(EC)
        end,
        fun(EC) ->
            [?_assertMatch(10.0, get_rate(EC)),
                ?_assertMatch(ok, set_rate(EC, 15)),
                ?_assertMatch(15, get_rate(EC)),
                ?_assertMatch(ok, set_rate(EC, 29.97)),
                ?_assertMatch(29.97, get_rate(EC)),
                ?_assertMatch(bad_param, set_rate(EC, -100.0)),
                ?_assertMatch(29.97, get_rate(EC))]
        end
    }}.

%%-----------------------------------------------------------------------------
%% @doc Test getting and setting the rate of a started EC.
%% @spec get_and_set_rate_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_and_set_rate_started_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_and_set_rate_started_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "get_and_set_rate_started_test_()"),
            {ok, EC} = start_fsm([{"rate", "10.0"}]),
            ok = start(EC),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = add_component(EC, RTC),
            mock_rtc:cbs(RTC),
            {EC, RTC}
        end,
        fun({EC, RTC}) ->
            ok = remove_component(EC, RTC),
            ok = mock_rtc:stop_fsm(RTC),
            ok = stop(EC),
            stop_fsm(EC)
        end,
        fun({EC, RTC}) ->
            [?_assertMatch(10.0, get_rate(EC)),
                ?_assertMatch([], mock_rtc:cbs(RTC)),
                ?_assertMatch(ok, set_rate(EC, 15)),
                ?_assertMatch([on_rate_changed], mock_rtc:cbs(RTC)),
                ?_assertMatch(15, get_rate(EC)),
                ?_assertMatch(ok, set_rate(EC, 29.97)),
                ?_assertMatch([on_rate_changed], mock_rtc:cbs(RTC)),
                ?_assertMatch(29.97, get_rate(EC)),
                ?_assertMatch(bad_param, set_rate(EC, -100.0)),
                ?_assertMatch([], mock_rtc:cbs(RTC)),
                ?_assertMatch(29.97, get_rate(EC))]
        end
    }}.

%%-----------------------------------------------------------------------------
%% @doc Test getting the EC kind.
%% @spec get_kind_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_kind_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_kind_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "get_kind_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            EC
        end,
        fun(EC) ->
            stop_fsm(EC)
        end,
        fun(EC) ->
            [?_assertMatch(periodic, get_kind(EC))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test getting the EC profile.
%% @spec get_profile_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_profile_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_profile_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "get_profile_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = set_owner(EC, RTC),
            ok = add_component(EC, RTC),
            {EC, RTC}
        end,
        fun({EC, RTC}) ->
            ok = remove_component(EC, RTC),
            mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, RTC}) ->
            [?_assertEqual(#ec_profile{kind=periodic, rate=1.0, owner=RTC,
                        parts=[RTC], props=[{"rate", "1"}]},
                    get_profile(EC))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test getting the EC participants.
%% @spec get_parts_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_parts_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_parts_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "get_parts_test_()"),
            {ok, EC} = start_fsm([{"rate", "1"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {EC, RTC}
        end,
        fun({EC, RTC}) ->
            ok = remove_component(EC, RTC),
            mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, RTC}) ->
            [?_assertMatch([], get_parts(EC)),
                ?_assertMatch(ok, add_component(EC, RTC)),
                ?_assertMatch(1, length(get_parts(EC))),
                ?_assertMatch(true, lists:keymember(RTC, 3, get_parts(EC)))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test changing the state of an EC with participants.
%% @spec start_and_stop_with_part_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(start_and_stop_with_part_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
start_and_stop_with_part_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "start_and_stop_with_part_test_()"),
            {ok, EC} = start_fsm([{"rate", "10.0"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = add_component(EC, RTC),
            mock_rtc:cbs(RTC),
            {EC, RTC}
        end,
        fun({EC, RTC}) ->
            ok = remove_component(EC, RTC),
            ok = mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, RTC}) ->
            [?_assertMatch(ok, start(EC)),
                ?_test(timer:sleep(100)),
                ?_assertMatch([on_startup], mock_rtc:cbs(RTC)),
                ?_assertMatch(true, is_running(EC)),
                ?_assertMatch(ok, stop(EC)),
                ?_test(timer:sleep(100)),
                ?_assertMatch([on_shutdown], mock_rtc:cbs(RTC)),
                ?_assertMatch(false, is_running(EC))]
        end
    }}.


%%-----------------------------------------------------------------------------
%% @doc Test handling a participant that dies prematurely.
%% @spec ec_part_premature_death_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(ec_part_premature_death_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
ec_part_premature_death_test_() ->
    {inorder, {setup,
        fun() ->
            ?LOG(rtl_paranoid, "ec_part_premature_death_test_()"),
            {ok, EC} = start_link_fsm([{"rate", "10.0"}]),
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            ok = add_component(EC, RTC),
            mock_rtc:cbs(RTC),
            {_, ECP, RTC} = lists:keyfind(RTC, 3, ec:get_parts(EC)),
            {EC, ECP, RTC}
        end,
        fun({EC, _ECP, RTC}) ->
            ok = reset_component(EC, RTC),
            ok = remove_component(EC, RTC),
            ok = mock_rtc:stop_fsm(RTC),
            stop_fsm(EC)
        end,
        fun({EC, ECP, _RTC}) ->
            [?_test(exit(ECP, died_for_a_test)),
                ?_assertMatch(true, is_process_alive(EC))]
        end
    }}.

-endif. % TEST

