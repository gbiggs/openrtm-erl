%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Helper process to initialise an RTC.
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
-module(rtc_exiter).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include("type_specs.hrl").
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
-export([start_link/2]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([helper_func/2]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the helper process and link to it.
%% @spec start_link(Helpee, Args) -> {ok, Pid} | {error, Reason}
%% where
%%      Helpee = pid()
%%      Args = [ECs]
%%          PIDs of the execution contexts to deactivate in and detach from.
%%      Pid = pid()
%%      Reason = any()
%% @end
-spec(start_link(Helpee::pid(), Args::[ECs::[pid()]]) ->
    {ok, Pid::pid()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
start_link(Helpee, Args) ->
    Pid = spawn_link(rtc_exiter, helper_func, [Helpee | Args]),
    {ok, Pid}.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Helper process main function. Deactivates in and detaches from each EC.
%% @spec helper_func(Helpee, ECs) -> ok
%% where
%%      Helpee = pid()
%%          PID of the process being helped (so we can send an event to it).
%%      ECs = [pid() | object_ref()]
%%      Ports = [pid()]
%% @end
-spec(helper_func(Helpee::pid(), ECs::[pid() | object_ref()]) -> ok).
%%-----------------------------------------------------------------------------
helper_func(Helpee, ECs) ->
    ?LOG(rtl_debug, "rtc_exiter starting, RTC: ~p, ECs: ~p", [Helpee, ECs]),
    lists:foreach(fun(EC) -> shift_to_inactive(EC, Helpee) end, ECs),
    lists:foreach(fun(EC) -> call_remove_component(EC, Helpee) end, ECs),
    notify_helpee(Helpee).


%%-----------------------------------------------------------------------------
%% @doc Shift an RTC to the inactive state if it is not already there.
%% @spec shift_to_inactive(EC, RTC) -> ok
%% where
%%      EC = pid() | object_ref()
%%      RTC = pid()
%% @end
-spec(shift_to_inactive(EC::pid() | object_ref(), RTC::pid()) -> ok).
%%-----------------------------------------------------------------------------
shift_to_inactive(EC, RTC) ->
    case call_get_component_state(EC, RTC)
        of active ->
            ?LOG(rtl_debug, "Deactivating RTC ~p in EC ~p", [RTC, EC]),
            call_deactivate_component(EC, RTC)
         ; error ->
            ?LOG(rtl_debug, "Resetting RTC ~p in EC ~p", [RTC, EC]),
            call_reset_component(EC, RTC)
         ; inactive ->
            ?LOG(rtl_debug, "RTC ~p is already inactive in EC ~p", [RTC, EC]),
            ok
    end.


%%-----------------------------------------------------------------------------
%% @doc Notify the helpee of the result.
%% @spec notify_helpee(Helpee) -> ok
%% where
%%      Helpee = pid()
%% @end
-spec(notify_helpee(Helpee::pid()) -> ok).
%%-----------------------------------------------------------------------------
notify_helpee(Helpee) ->
    ?LOG(rtl_debug, "rtc_exiter notifying ~p that it is finished.", [Helpee]),
    gen_fsm:send_event(Helpee, {exiting, self()}).


%%-----------------------------------------------------------------------------
%% @doc Call remove_component on a local or CORBA reference.
%% @spec call_remove_component(EC, RTC) -> rtc_cb_return()
%% where
%%      EC = pid() | object_ref()
%%      RTC = pid()
%% @end
-spec(call_remove_component(EC::pid() | object_ref(), RTC::pid()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_remove_component(EC, RTC) when is_pid(EC) ->
    ec:remove_component(EC, RTC);
call_remove_component({'RTC_ExecutionContextService', _, _, _, _, _}=EC,
        RTC) ->
    utils:from_rtc_rc('RTC_ExecutionContextService':remove_component(EC,
            rtc:get_corba_obj(RTC))).


%%-----------------------------------------------------------------------------
%% @doc Call get_component_state on a local or CORBA reference.
%% @spec call_get_component_state(EC, RTC) -> rtc_lifecyclestate()
%% where
%%      EC = pid() | object_ref()
%%      RTC = pid()
%% @end
-spec(call_get_component_state(EC::pid() | object_ref(), RTC::pid()) ->
    rtc_lifecyclestate()).
%%-----------------------------------------------------------------------------
call_get_component_state(EC, RTC) when is_pid(EC) ->
    ec:get_component_state(EC, RTC);
call_get_component_state({'RTC_ExecutionContextService', _, _, _, _, _}=EC,
        RTC) ->
    case 'RTC_ExecutionContextService':get_component_state(EC,
            rtc:get_corba_obj(RTC))
        of 'INACTIVE_STATE' ->
            inactive
         ; 'ACTIVE_STATE' ->
            active
         ; 'ERROR_STATE' ->
            error
         ; 'CREATED_STATE' ->
            bad_param
    end.


%%-----------------------------------------------------------------------------
%% @doc Call deactivate_component on a local or CORBA reference.
%% @spec call_deactivate_component(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_deactivate_component(RTC::pid() | object_ref(),
        Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_deactivate_component(EC, RTC) when is_pid(EC) ->
    ec:deactivate_component(EC, RTC);
call_deactivate_component({'RTC_ExecutionContextService', _, _, _, _, _}=EC,
        RTC) ->
    utils:from_rtc_rc('RTC_ExecutionContextService':deactivate_component(EC,
            rtc:get_corba_obj(RTC))).


%%-----------------------------------------------------------------------------
%% @doc Call reset_component on a local or CORBA reference.
%% @spec call_reset_component(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_reset_component(RTC::pid() | object_ref(),
        Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_reset_component(EC, RTC) when is_pid(EC) ->
    ec:reset_component(EC, RTC);
call_reset_component({'RTC_ExecutionContextService', _, _, _, _, _}=EC,
        RTC) ->
    utils:from_rtc_rc('RTC_ExecutionContextService':reset_component(EC,
            rtc:get_corba_obj(RTC))).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

