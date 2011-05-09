%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Execution context Erlang interface.
%% @reference <a href="http://www.openrtm.org/">OpenRTM.org</a>
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
-module(ec).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("ec.hrl").
-include("rtc.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Start here
-export([create/1, destroy/1]).
% ExecutionContextOperations interface.
-export([is_running/1, start/1, stop/1, get_rate/1, set_rate/2,
        add_component/2, remove_component/2, activate_component/2,
        deactivate_component/2, reset_component/2, get_component_state/2,
        get_kind/1]).
% ExecutionContextService interface.
-export([get_profile/1]).
% Extra interface.
-export([get_owner/1, set_owner/2, get_parts/1, get_corba_obj/1,
        set_corba_obj/3, state/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create a new execution context, including supervisor, FSM and CORBA
%% interface.
%% @spec create(Config) -> {ok, Supervisor, FSM} | {error, Reason}
%% where
%%      Config = config()
%%          The configuration of the execution context.
%%      Supervisor = pid()
%%          The top-level supervisor of the EC's personal supervision tree.
%%          You will need this to place the EC into a parent supervisor.
%%      FSM = pid()
%%          The FSM implementing the EC behaviour. Pass this to the functions
%%          exported by this module.
%%      Reason = any()
%% @end
-spec(create(Config::config()) -> {ok, Supervisor::pid(), FSM::pid()}).
%%-----------------------------------------------------------------------------
create(Config) ->
    case ec_sup:start_link(Config)
        of {ok, S} ->
            {fsm, FSM, worker, _} = lists:keyfind(fsm, 1,
                supervisor:which_children(S)),
            {corba, CORBA, worker, _} = lists:keyfind(corba, 1,
                supervisor:which_children(S)),
            set_corba_obj(FSM, CORBA, corba_obj_mgr:get_obj(CORBA)),
            corba_obj_mgr:send_info(CORBA, {set_target, FSM}),
            {ok, S, FSM}
         ; {error, Reason} ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Destroy an execution context, from its supervisor on down.
%% @spec destroy(S) -> ok
%% where
%%      S = pid()
%%          PID of the EC's top supervisor, as returned by create/1.
%% @end
-spec(destroy(S::pid()) -> ok).
%%-----------------------------------------------------------------------------
destroy(S) ->
    true = exit(S, normal),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Check if the EC is in the running state.
%% @spec is_running(EC) -> boolean()
%% where
%%      EC = pid()
%% @end
-spec(is_running(EC::pid()) -> boolean()).
%%-----------------------------------------------------------------------------
is_running(EC) ->
    gen_fsm:sync_send_event(EC, is_running).


%%-----------------------------------------------------------------------------
%% @doc Tell the EC to enter the running state.
%% @spec start(EC) -> ok | precon_not_met
%% where
%%      EC = pid()
%% @end
-spec(start(EC::pid()) -> ok | precon_not_met).
%%-----------------------------------------------------------------------------
start(EC) ->
    case state(EC)
        of started ->
            precon_not_met
         ; stopped ->
            gen_fsm:send_event(EC, start)
    end.


%%-----------------------------------------------------------------------------
%% @doc Tell the EC to enter the stopped state.
%% @spec stop(EC) -> ok | precon_not_met
%% where
%%      EC = pid()
%% @end
-spec(stop(EC::pid()) -> ok | precon_not_met).
%%-----------------------------------------------------------------------------
stop(EC) ->
    case state(EC)
        of stopped ->
            precon_not_met
         ; started ->
            gen_fsm:send_event(EC, stop)
    end.


%%-----------------------------------------------------------------------------
%% @doc Get the execution rate of an EC.
%% @spec get_rate(EC) -> float()
%% where
%%      EC = pid()
%% @end
-spec(get_rate(EC::pid()) -> Rate::float()).
%%-----------------------------------------------------------------------------
get_rate(EC) ->
    gen_fsm:sync_send_all_state_event(EC, get_rate).


%%-----------------------------------------------------------------------------
%% @doc Set the execution rate of an EC.
%% @spec set_rate(EC, NewRate) -> ok | {error, bad_rate}
%% where
%%      EC = pid()
%%      NewRate = float()
%%      Reason = any()
%% @end
-spec(set_rate(EC::pid(), NewRate::float()) -> ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
set_rate(EC, NewRate) ->
    gen_fsm:sync_send_all_state_event(EC, {set_rate, NewRate}).


%%-----------------------------------------------------------------------------
%% @doc Add a component to this EC as a participant.
%% @spec add_component(EC, RTC) -> ok | {error, Reason}
%% where
%%      EC = pid()
%%      RTC = pid() | atom()
%%      Reason = any()
%% @end
-spec(add_component(EC::pid(), RTC::pid() | atom()) ->
    ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
add_component(EC, RTC) ->
    gen_fsm:sync_send_all_state_event(EC, {add_comp, RTC}).


%%-----------------------------------------------------------------------------
%% @doc Remove a component from this EC's list of participants.
%% @spec remove_component(EC, RTC) -> ok | {error, Reason}
%% where
%%      EC = pid()
%%      RTC = pid() | atom()
%%      Reason = any()
%% @end
-spec(remove_component(EC::pid(), RTC::pid() | atom()) ->
    ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
remove_component(EC, RTC) ->
    gen_fsm:sync_send_all_state_event(EC, {rem_comp, RTC}).


%%-----------------------------------------------------------------------------
%% @doc Activate a participating component.
%% @spec activate_component(EC, RTC) -> ok | {error, Reason}
%% where
%%      EC = pid()
%%      RTC = pid() | atom()
%%      Reason = any()
%% @end
-spec(activate_component(EC::pid(), RTC::pid() | atom()) ->
    ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
activate_component(EC, RTC) ->
    gen_fsm:sync_send_all_state_event(EC, {act_comp, RTC}).


%%-----------------------------------------------------------------------------
%% @doc Deactivate a participating component.
%% @spec deactivate_component(EC, RTC) -> ok | {error, Reason}
%% where
%%      EC = pid()
%%      RTC = pid() | atom()
%%      Reason = any()
%% @end
-spec(deactivate_component(EC::pid(), RTC::pid() | atom()) ->
    ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
deactivate_component(EC, RTC) ->
    gen_fsm:sync_send_all_state_event(EC, {deact_comp, RTC}).


%%-----------------------------------------------------------------------------
%% @doc Reset a participating component.
%% @spec reset_component(EC, RTC) -> ok | {error, Reason}
%% where
%%      EC = pid()
%%      RTC = pid() | atom()
%%      Reason = any()
%% @end
-spec(reset_component(EC::pid(), RTC::pid() | atom()) ->
    ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
reset_component(EC, RTC) ->
    gen_fsm:sync_send_all_state_event(EC, {reset_comp, RTC}).


%%-----------------------------------------------------------------------------
%% @doc Get the life cycle state of a component.
%% @spec get_component_state(EC, RTC) -> rtc_lifecyclestate()
%% where
%%      EC = pid()
%%      RTC = pid() | atom()
%% @end
-spec(get_component_state(EC::pid(), RTC::pid() | atom()) ->
    rtc_lifecyclestate()).
%%-----------------------------------------------------------------------------
get_component_state(EC, RTC) ->
    gen_fsm:sync_send_all_state_event(EC, {get_state, RTC}).


%%-----------------------------------------------------------------------------
%% @doc Get the execution context kind of an EC.
%% @spec get_kind(EC) -> ec_kind()
%% where
%%      EC = pid()
%% @end
-spec(get_kind(EC::pid()) -> ec_kind()).
%%-----------------------------------------------------------------------------
get_kind(EC) ->
    gen_fsm:sync_send_all_state_event(EC, get_kind).


%%-----------------------------------------------------------------------------
%% @doc Get the execution context profile of an EC.
%% @spec get_profile(EC) -> ec_profile()
%% where
%%      EC = pid()
%% @end
-spec(get_profile(EC::pid()) -> #ec_profile{}).
%%-----------------------------------------------------------------------------
get_profile(EC) ->
    gen_fsm:sync_send_all_state_event(EC, get_profile).


%%-----------------------------------------------------------------------------
%% @doc Get the owner of an EC.
%% @spec get_owner(EC) -> Owner
%% where
%%      EC = pid()
%%      Owner = pid()
%% @end
-spec(get_owner(EC::pid()) -> Owner::pid()).
%%-----------------------------------------------------------------------------
get_owner(EC) ->
    gen_fsm:sync_send_all_state_event(EC, get_owner).


%%-----------------------------------------------------------------------------
%% @doc Set the owner of an execution context.
%% @spec set_owner(EC, Owner) -> ok
%% where
%%      EC = pid()
%%      Owner = pid()
%% @end
-spec(set_owner(EC::pid(), Owner::pid()) -> ok).
%%-----------------------------------------------------------------------------
set_owner(EC, Owner) ->
    gen_fsm:sync_send_all_state_event(EC, {set_owner, Owner}).


%%-----------------------------------------------------------------------------
%% @doc Get the participants of an EC.
%% @spec get_parts(EC) -> Participants
%% where
%%      EC = pid()
%%      Participants = [pid()]
%% @end
-spec(get_parts(EC::pid()) -> Participants::[pid()]).
%%-----------------------------------------------------------------------------
get_parts(EC) ->
    gen_fsm:sync_send_all_state_event(EC, get_parts).


%%-----------------------------------------------------------------------------
%% @doc Get the CORBA object providing the interface for an EC.
%% @spec get_corba_obj(EC) -> {Pid, Obj}
%% where
%%      EC = pid()
%%      Pid = pid()
%%      Obj = object_ref()
%% @end
-spec(get_corba_obj(EC::pid()) -> {Pid::pid(), Obj::object_ref()}).
%%-----------------------------------------------------------------------------
get_corba_obj(EC) ->
    gen_fsm:sync_send_all_state_event(EC, get_corba_obj).


%%-----------------------------------------------------------------------------
%% @doc Set the CORBA object providing the interface for an EC.
%% @spec set_corba_obj(EC, Pid, Obj) -> ok
%% where
%%      EC = pid()
%%      Pid = pid()
%%      Obj = object_ref()
%% @end
-spec(set_corba_obj(EC::pid(), Pid::pid(), Obj::object_ref()) -> ok).
%%-----------------------------------------------------------------------------
set_corba_obj(EC, Pid, Obj) ->
    gen_fsm:sync_send_all_state_event(EC, {set_corba_obj, Pid, Obj}).


%%-----------------------------------------------------------------------------
%% @doc Get the current state.
%% @spec state(FSM) -> atom()
%% where
%%      FSM = pid()
%% @end
-spec(state(FSM::pid()) -> atom()).
%%-----------------------------------------------------------------------------
state(FSM) ->
    gen_fsm:sync_send_event(FSM, state).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

