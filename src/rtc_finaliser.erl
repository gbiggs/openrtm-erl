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
-module(rtc_finaliser).


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
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/2]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([helper_func/4]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the helper process and link to it.
%% @spec start_link(Helpee, Args) -> {ok, Pid} | {error, Reason}
%% where
%%      Helpee = pid()
%%      Args = [ECs, Ports, Result]
%%          PIDs of the execution contexts and ports to shut down and the
%%          result to forward to the helpee.
%%      Pid = pid()
%%      Reason = any()
%% @end
-spec(start_link(Helpee::pid(), Args::[any()]) ->
    {ok, Pid::pid()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
start_link(Helpee, Args) ->
    Pid = spawn_link(rtc_finaliser, helper_func, [Helpee | Args]),
    {ok, Pid}.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Helper process main function. Stops each EC and port.
%% @spec helper_func(Helpee, ECs, Result) -> ok
%% where
%%      Helpee = pid()
%%          PID of the process being helped (so we can send an event to it).
%%      ECs = [pid()]
%%      Ports = [pid()]
%%      Result = rtc_cb_return()
%% @end
-spec(helper_func(Helpee::pid(), ECs::[pid()], Ports::[pid()],
        Result::rtc_cb_return()) -> ok).
%%-----------------------------------------------------------------------------
helper_func(Helpee, ECs, Ports, Result) ->
    ?LOG(rtl_debug,
        "rtc_finaliser starting, RTC: ~p, ECs: ~p, Ports: ~p, Result: ~p",
        [Helpee, ECs, Ports, Result]),
    lists:foreach(fun call_stop/1, ECs),
    lists:foreach(fun port:stop/1, Ports),
    notify_helpee(Helpee, Result).


%%-----------------------------------------------------------------------------
%% @doc Notify the helpee of the result.
%% @spec notify_helpee(Helpee, Result) -> ok
%% where
%%      Helpee = pid()
%%      Result = rtc_cb_return()
%% @end
-spec(notify_helpee(Helpee::pid(), Result::rtc_cb_return()) -> ok).
%%-----------------------------------------------------------------------------
notify_helpee(Helpee, Result) ->
    ?LOG(rtl_debug, "rtc_finaliser notifying ~p that it is finished.",
        [Helpee]),
    gen_fsm:send_event(Helpee, {finalise_done, Result, self()}).


%%-----------------------------------------------------------------------------
%% @doc Call stop on a local or CORBA reference.
%% @spec call_stop(EC) -> rtc_cb_return()
%% where
%%      EC = pid() | object_ref()
%% @end
-spec(call_stop(EC::pid() | object_ref()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_stop(EC) when is_pid(EC) ->
    ec:stop(EC);
call_stop({'RTC_ExecutionContextService', _, _, _, _, _}=EC) ->
    utils:from_rtc_rc('RTC_ExecutionContextService':stop(EC)).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

