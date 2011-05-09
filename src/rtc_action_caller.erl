%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Helper process to call an action callback safely.
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
-module(rtc_action_caller).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("ec.hrl").


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
-export([helper_func/7]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the helper process and link to it.
%% @spec start_link(Helpee, Args) -> {ok, Pid} | {error, Reason}
%% where
%%      Helpee = pid()
%%      Args = [Action, Module, EC, RTC, Data]
%%      Action = execute | state_update | rate_changed | startup |
%%          shutdown | activate | deactivate | aborting | error | reset
%%      Module = atom()
%%          The callback module.
%%      EC = ec_handle()
%%      Data = any()
%%          Data created and used by the callbacks.
%%      Pid = pid()
%%      Reason = any()
%% @end
-spec(start_link(Helpee::pid(), Args::[]) ->
    {ok, Pid::pid()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
start_link(Helpee, Args) ->
    Pid = spawn_link(rtc_action_caller, helper_func, [Helpee | Args]),
    {ok, Pid}.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Helper process main function. Calls a callback.
%% @spec helper_func(Helpee, Action, RTC, PM, EC, Data) -> ok
%% where
%%      Helpee = pid()
%%          PID of the process being helped (so we can send an event to it).
%%      Action = execute | state_update | rate_changed | startup |
%%          shutdown | activate | deactivate | aborting | error | reset
%%      Module = atom()
%%          The callback module.
%%      RTC = pid()
%%      PM = pid()
%%          The RTC's port manager.
%%      EC = ec_handle()
%%      Data = any()
%%          Data created and used by the callbacks.
%% @end
-spec(helper_func(Helpee::pid(), Action::execute | state_update |
        rate_changed | startup | shutdown | activate | deactivate | aborting |
        error | reset, Module::atom(), RTC::pid(), PM::pid(), EC::ec_handle(),
        Data::any()) ->
    ok).
%%-----------------------------------------------------------------------------
helper_func(Helpee, Action, Module, RTC, PM, EC, Data) ->
    {Result, NewData} = call_action(Action, Module, RTC, PM, EC, Data),
    notify_helpee(Helpee, Result, NewData).


%%-----------------------------------------------------------------------------
%% @doc Call an action callback.
%% @spec call_action(Action, Module, RTC, PM, EC, Data) -> {Result, NewData}
%% where
%%      Action = on_execute | on_state_update | on_rate_changed | on_startup |
%%          on_shutdown | on_activate | on_deactivate | on_aborting |
%%          on_error | on_reset
%%      Module = atom()
%%      RTC = pid()
%%      PM = pid()
%%      EC = ec_handle()
%%      Result = rtc_cb_return()
%%      NewData = any()
%% @end
-spec(call_action(Action::on_execute | on_state_update | on_rate_changed |
        on_startup | on_shutdown | on_activated | on_deactivated |
        on_aborting | on_error | on_reset,
        Module::atom(), RTC::pid(), PM::pid(), EC::ec_handle(), Data::any()) ->
    {Result::rtc_cb_return(), NewData::any()}).
%%-----------------------------------------------------------------------------
call_action(on_execute, Module, RTC, PM, EC, Data) ->
    apply(Module, on_execute, [RTC, PM, EC, Data]);
call_action(on_state_update, Module, RTC, PM, EC, Data) ->
    apply(Module, on_state_update, [RTC, PM, EC, Data]);
call_action(on_rate_changed, Module, RTC, PM, EC, Data) ->
    apply(Module, on_rate_changed, [RTC, PM, EC, Data]);
call_action(on_startup, Module, RTC, PM, EC, Data) ->
    apply(Module, on_startup, [RTC, PM, EC, Data]);
call_action(on_shutdown, Module, RTC, PM, EC, Data) ->
    apply(Module, on_shutdown, [RTC, PM, EC, Data]);
call_action(on_activated, Module, RTC, PM, EC, Data) ->
    apply(Module, on_activated, [RTC, PM, EC, Data]);
call_action(on_deactivated, Module, RTC, PM, EC, Data) ->
    apply(Module, on_deactivated, [RTC, PM, EC, Data]);
call_action(on_aborting, Module, RTC, PM, EC, Data) ->
    apply(Module, on_aborting, [RTC, PM, EC, Data]);
call_action(on_error, Module, RTC, PM, EC, Data) ->
    apply(Module, on_error, [RTC, PM, EC, Data]);
call_action(on_reset, Module, RTC, PM, EC, Data) ->
    apply(Module, on_reset, [RTC, PM, EC, Data]).


%%-----------------------------------------------------------------------------
%% @doc Notify the helpee of the result of the callback.
%% @spec notify_helpee(Helpee, Result, NewData) -> ok
%% where
%%      Helpee = pid()
%%      Result = rtc_cb_return()
%%      NewData = any()
%% @end
-spec(notify_helpee(Helpee::pid(), Result::rtc_cb_return(), NewData::any()) ->
    ok).
%%-----------------------------------------------------------------------------
notify_helpee(Helpee, Result, NewData) ->
    gen_fsm:send_event(Helpee, {cb_done, Result, NewData, self()}).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

