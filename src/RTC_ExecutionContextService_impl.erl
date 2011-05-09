%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc CORBA wrapper around the execution context interfaces.
%% @reference <a href="http://www.openrtm.org/">OpenRTM.org</a>
%% @source idl/RTC.idl
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
-module('RTC_ExecutionContextService_impl').

%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include("idl/RTM.hrl").
-include("idl/RTC.hrl").
-include("idl/SDOPackage.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("ec.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------
-record(ec_svc, {target=nil :: pid() | nil % Target process for the interface
    }).


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/1, stop_intf/1]).
-export([get_profile/2]).
-export([is_running/2, start/2, stop/2, get_rate/2, set_rate/3, get_kind/2]).
-export([add_component/3, remove_component/3, activate_component/3,
        deactivate_component/3, reset_component/3, get_component_state/3]).

%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_info/2, terminate/2, code_change/3]).

%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the CORBA ExecutionContextService interface.
%% @spec start_link([]) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link([]) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link([]) ->
    'RTC_ExecutionContextService':oe_create_link(none,
        [{sup_child, true}, {persistent, true}]).


%%-----------------------------------------------------------------------------
%% @doc Stop the CORBA ExecutionContextService interface.
%% @spec stop_intf(Pid) -> ok
%% where
%%      Pid = pid(
%%          The PID of the process running the interface.
%% @end
-spec(stop_intf(Pid::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_intf(Pid) ->
    gen_server:cast(Pid, stop).


%%-----------------------------------------------------------------------------
%% @doc Get the profile of the EC.
%% @spec get_profile(This, State) -> #'RTC_ExecutionContextProfile'{}
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%% @end
-spec(get_profile(This::object_ref(), State::#ec_svc{}) ->
    #'RTC_ExecutionContextProfile'{}).
%%-----------------------------------------------------------------------------
get_profile(_This, #ec_svc{target=T}=State) ->
    Objectifier = fun(RTC) when is_pid(RTC) ->
            rtc:get_corba_obj(RTC);
        ({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC) ->
            RTC
    end,
    Prof = ec:get_profile(T),
    ?LOG(rtl_paranoid, "Raw profile is ~p", [Prof]),
    Owner = case Prof#ec_profile.owner
        of nil ->
            ?ORBER_NIL_OBJREF
         ; Pid ->
            rtc:get_corba_obj(Pid)
    end,
    Result = #'RTC_ExecutionContextProfile'{
        kind=to_corba_kind(Prof#ec_profile.kind),
        rate=Prof#ec_profile.rate,
        owner=Owner,
        participants=lists:map(Objectifier, Prof#ec_profile.parts),
        properties=nvlist:from_list(Prof#ec_profile.props)
    },
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Check if the EC is in the running state.
%% @spec is_running(This, State) -> boolean()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%% @end
-spec(is_running(This::object_ref(), State::#ec_svc{}) -> boolean()).
%%-----------------------------------------------------------------------------
is_running(_This, #ec_svc{target=T}=State) ->
    {reply, ec:is_running(T), State}.


%%-----------------------------------------------------------------------------
%% @doc Send the EC to the running state.
%% @spec start(This, State) -> rtc_returncode()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%% @end
-spec(start(This::object_ref(), State::#ec_svc{}) -> rtc_returncode()).
%%-----------------------------------------------------------------------------
start(_This, #ec_svc{target=T}=State) ->
    {reply, utils:to_rtc_rc(ec:start(T)), State}.


%%-----------------------------------------------------------------------------
%% @doc Send the EC to the stopped state.
%% @spec stop(This, State) -> rtc_returncode()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%% @end
-spec(stop(This::object_ref(), State::#ec_svc{}) -> rtc_returncode()).
%%-----------------------------------------------------------------------------
stop(_This, #ec_svc{target=T}=State) ->
    {reply, utils:to_rtc_rc(ec:stop(T)), State}.


%%-----------------------------------------------------------------------------
%% @doc Get the rate in Hertz at which the EC is cycling.
%% @spec get_rate(This, State) -> double()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%% @end
-spec(get_rate(This::object_ref(), State::#ec_svc{}) -> float()).
%%-----------------------------------------------------------------------------
get_rate(_This, #ec_svc{target=T}=State) ->
    {reply, ec:get_rate(T), State}.


%%-----------------------------------------------------------------------------
%% @doc Set the rate in Hertz at which the EC should cycle.
%% @spec set_rate(This, State, Rate) -> rtc_returncode()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%%      Rate = float()
%% @end
-spec(set_rate(This::object_ref(), State::#ec_svc{}, Rate::float()) ->
    rtc_returncode()).
%%-----------------------------------------------------------------------------
set_rate(_This, #ec_svc{target=T}=State, Rate) ->
    {reply, utils:to_rtc_rc(ec:set_rate(T, Rate)), State}.


%%-----------------------------------------------------------------------------
%% @doc Make a component participate in the EC.
%% @spec add_component(This, State, Comp) -> rtc_returncode()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%%      Comp = object_ref()
%% @end
-spec(add_component(This::object_ref(), State::#ec_svc{},
        Comp::object_ref()) ->
    rtc_returncode()).
%%-----------------------------------------------------------------------------
add_component(_This, #ec_svc{target=T}=State, Comp) ->
    {reply, utils:to_rtc_rc(ec:add_component(T, Comp)), State}.


%%-----------------------------------------------------------------------------
%% @doc Stop an RTC from participating in the EC.
%% @spec remove_component(This, State, Comp) -> rtc_returncode()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%%      Comp = object_ref()
%% @end
-spec(remove_component(This::object_ref(), State::#ec_svc{},
        Comp::object_ref()) ->
    rtc_returncode()).
%%-----------------------------------------------------------------------------
remove_component(_This, #ec_svc{target=T}=State, Comp) ->
    {reply, utils:to_rtc_rc(ec:remove_component(T, Comp)), State}.


%%-----------------------------------------------------------------------------
%% @doc Send a component to the active state within the EC.
%% @spec activate_component(This, State, Comp) -> rtc_returncode()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%%      Comp = object_ref()
%% @end
-spec(activate_component(This::object_ref(), State::#ec_svc{},
        Comp::object_ref()) ->
    rtc_returncode()).
%%-----------------------------------------------------------------------------
activate_component(_This, #ec_svc{target=T}=State, Comp) ->
    {reply, utils:to_rtc_rc(ec:activate_component(T, Comp)), State}.


%%-----------------------------------------------------------------------------
%% @doc Send a component to the inactive state within the EC.
%% @spec deactivate_component(This, State, Comp) -> rtc_returncode()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%%      Comp = object_ref()
%% @end
-spec(deactivate_component(This::object_ref(), State::#ec_svc{},
        Comp::object_ref()) ->
    rtc_returncode()).
%%-----------------------------------------------------------------------------
deactivate_component(_This, #ec_svc{target=T}=State, Comp) ->
    {reply, utils:to_rtc_rc(ec:deactivate_component(T, Comp)), State}.


%%-----------------------------------------------------------------------------
%% @doc Return a component to the inactive state from the error state.
%% @spec reset_component(This, State, Comp) -> rtc_returncode()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%%      Comp = object_ref()
%% @end
-spec(reset_component(This::object_ref(), State::#ec_svc{},
        Comp::object_ref()) ->
    rtc_returncode()).
%%-----------------------------------------------------------------------------
reset_component(_This, #ec_svc{target=T}=State, Comp) ->
    {reply, utils:to_rtc_rc(ec:reset_component(T, Comp)), State}.


%%-----------------------------------------------------------------------------
%% @doc Get the lifecycle state of a component in the EC.
%% @spec get_component_state(This, State, Comp) -> lifecyclestate()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%%      Comp = object_ref()
%% @end
-spec(get_component_state(This::object_ref(), State::#ec_svc{},
        Comp::object_ref()) ->
    lifecyclestate()).
%%-----------------------------------------------------------------------------
get_component_state(_This, #ec_svc{target=T}=State, Comp) ->
    Result = case ec:get_component_state(T, Comp)
        of inactive ->
            'INACTIVE_STATE'
         ; active ->
            'ACTIVE_STATE'
         ; error ->
            'ERROR_STATE'
         ; bad_param ->
            'CREATED_STATE'
    end,
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the kind of the execution context.
%% @spec get_kind(This, State) -> executionkind()
%% where
%%      This = object_ref()
%%      State = #ec_svc{}
%% @end
-spec(get_kind(This::object_ref(), State::#ec_svc{}) -> executionkind()).
%%-----------------------------------------------------------------------------
get_kind(_This, #ec_svc{target=T}=State) ->
    {reply, to_corba_kind(ec:get_kind(T)), State}.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc gen_server initialisation callback function.
%% @spec init(none) -> {ok, State} | {stop, Reason}
%% where
%%      State = nil
%%      Reason = any()
%% @end
-spec(init(none) -> {ok, State::#ec_svc{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init(none) ->
    ?LOG(rtl_info, "Initialising CORBA ExecutionContextService interface."),
    {ok, #ec_svc{}}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = {set_target, Target}
%%      Target = pid()
%%      State = #ec_svc{}
%%      NewState = #ec_svc{}
%% @end
-spec(handle_info(Info::{set_target, Target::pid()}, State::#ec_svc{}) ->
    {noreply, NewState::#ec_svc{}}).
%%-----------------------------------------------------------------------------
handle_info({set_target, Target}, State) ->
    ?LOG(rtl_paranoid, "Received EC target: ~p", [Target]),
    {noreply, State#ec_svc{target=Target}}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any} | any()
%%      State = #ec_svc{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#ec_svc{}) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, _State) ->
    ?LOG(rtl_info, "Shutting down normally.");
terminate(shutdown, _State) ->
    ?LOG(rtl_info, "Shut down by supervisor.");
terminate({shutdown, Reason}, _State) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]);
terminate(Reason, _State) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = #ec_svc{}
%%      Extra = any()
%%      NewState = #ec_svc{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#ec_svc{},
        Extra::any()) -> {ok, NewState::#ec_svc{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Convert a local EC kind into the CORBA data type
%% @spec to_corba_kind(ec_kind()) -> executionkind()
%% @end
-spec(to_corba_kind(ec_kind()) -> executionkind()).
%%-----------------------------------------------------------------------------
to_corba_kind(periodic) ->
    'PERIODIC';
to_corba_kind(event_driven) ->
    'EVENT_DRIVEN';
to_corba_kind(other) ->
    'OTHER'.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

