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
-module(rtc_initialiser).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").


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
%%          PIDs of the execution contexts to add to.
%%      Pid = pid()
%%      Reason = any()
%% @end
-spec(start_link(Helpee::pid(), Args::[ECs::[pid()]]) ->
    {ok, Pid::pid()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
start_link(Helpee, Args) ->
    Pid = spawn_link(rtc_initialiser, helper_func, [Helpee | Args]),
    {ok, Pid}.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Helper process main function. Calls add_component on each EC.
%% @spec helper_func(Helpee, ECs) -> ok
%% where
%%      Helpee = pid()
%%          PID of the process being helped (so we can send an event to it).
%%      ECs = [pid()]
%% @end
-spec(helper_func(Helpee::pid(), ECs::[pid()]) -> ok).
%%-----------------------------------------------------------------------------
helper_func(Helpee, ECs) ->
    Results = lists:map(fun(EC) -> ec:add_component(EC, Helpee) end, ECs),
    MergedResult = lists:foldl(fun merge_result/2, ok, Results),
    notify_helpee(Helpee, MergedResult).


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
    gen_fsm:send_event(Helpee, {init_done, Result, self()}).


%%-----------------------------------------------------------------------------
%% @doc Merge a result, making errors override successes.
%% @spec merge_result(R1, R2) -> R3
%% where
%%      R1 = ok | {error, Reason}
%%      R2 = ok | {error, Reason}
%%      R3 = ok | {error, Reason}
%%      Reason = any()
%% @end
-spec(merge_result(R1::ok | {error, Reason::any()},
        R2::ok | {error, Reason::any()}) ->
    R3::ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
merge_result(_, {error, _}=R) ->
    R;
merge_result({error, _}=R, _) ->
    R;
merge_result(ok, ok) ->
    ok.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test merging results.
%% @spec merge_result_test() -> ok
%% @end
-spec(merge_result_test() -> ok).
%%-----------------------------------------------------------------------------
merge_result_test() ->
    ?assertMatch(ok, merge_result(ok, ok)),
    ?assertMatch({error, failed}, merge_result({error, failed}, ok)),
    ?assertMatch({error, failed}, merge_result(ok, {error, failed})).

-endif. % TEST

