%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Mock RTC behaviour module with ports for tests.
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
-module(mock_rtc_behv_ports).
-behaviour(gen_server).


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
%% External exports
%%-----------------------------------------------------------------------------
% Server control
-export([start_link/0, stop_server/0]).
% Useful functions
-export([cbs/0]).
% RTC behaviour callbacks
-export([on_initialize/2, on_finalize/3, on_startup/4, on_shutdown/4,
        on_activated/4, on_deactivated/4, on_aborting/4, on_error/4,
        on_reset/4]).
% Data flow component action callbacks
-export([on_execute/4, on_state_update/4, on_rate_changed/4]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2,
    code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the server.
%% @spec start_link() ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link() ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, mock_rtc_behv_ports}, mock_rtc_behv_ports,
        [], []).


%%-----------------------------------------------------------------------------
%% @doc Stop the server.
%% @spec stop_server() -> ok
%% @end
-spec(stop_server() -> ok).
%%-----------------------------------------------------------------------------
stop_server() ->
    gen_server:cast(mock_rtc_behv_ports, stop).


%%-----------------------------------------------------------------------------
%% @doc Get and reset the current callback history.
%% @spec cbs() -> ok
%% @end
-spec(cbs() -> ok).
%%-----------------------------------------------------------------------------
cbs() ->
    gen_server:call(mock_rtc_behv_ports, cbs).


%%-----------------------------------------------------------------------------
%% @doc RTC behaviour callback: on_initialize
%% @spec on_initialize(RTC, PM) -> {ok, on_initialize_result}
%% where
%%      RTC = pid()
%%      PM = any()
%% @end
-spec(on_initialize(RTC::pid(), PM::any()) -> {ok, on_initialize_result}).
%%-----------------------------------------------------------------------------
on_initialize(RTC, PM) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_initialize ~p ~p", [RTC, PM]),
    gen_server:cast(mock_rtc_behv_ports, on_initialize),
    % Add some ports
    ok = port_mgr:add_port(PM, datain, "DataIn", "TimedLong", RTC, []),
    ok = port_mgr:add_port(PM, dataout, "DataOut", "TimedLong", RTC, []),
    {ok, on_initialize_result}.


%%-----------------------------------------------------------------------------
%% @doc RTC behaviour callback: on_finalize
%% @spec on_finalize(RTC, PM, Data) -> {ok, on_finalize_result}
%% where
%%      RTC = pid()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_finalize(RTC::pid(), PM::any(), Data::any()) ->
    {ok, on_finalize_result}).
%%-----------------------------------------------------------------------------
on_finalize(RTC, PM, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_finalize ~p ~p ~p",
        [RTC, PM, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_finalize),
    {ok, on_finalize_result}.


%%-----------------------------------------------------------------------------
%% @doc RTC behaviour callback: on_startup
%% @spec on_startup(RTC, PM, EC, Data) -> {ok, on_startup_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_startup(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_startup_result}).
%%-----------------------------------------------------------------------------
on_startup(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_startup ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_startup),
    {ok, on_startup_result}.


%%-----------------------------------------------------------------------------
%% @doc RTC behaviour callback: on_shutdown
%% @spec on_shutdown(RTC, PM, EC, Data) -> {ok, on_shutdown_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_shutdown(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_shutdown_result}).
%%-----------------------------------------------------------------------------
on_shutdown(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_shutdown ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_shutdown),
    {ok, on_shutdown_result}.


%%-----------------------------------------------------------------------------
%% @doc RTC behaviour callback: on_activated
%% @spec on_activated(RTC, PM, EC, Data) -> {ok, on_activated_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_activated(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_activated_result}).
%%-----------------------------------------------------------------------------
on_activated(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_activated ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_activated),
    {ok, on_activated_result}.


%%-----------------------------------------------------------------------------
%% @doc RTC behaviour callback: on_deactivated
%% @spec on_deactivated(RTC, PM, EC, Data) -> {ok, on_deactivated_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_deactivated(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_deactivated_result}).
%%-----------------------------------------------------------------------------
on_deactivated(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_deactivated ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_deactivated),
    {ok, on_deactivated_result}.


%%-----------------------------------------------------------------------------
%% @doc RTC behaviour callback: on_aborting
%% @spec on_aborting(RTC, PM, EC, Data) -> {ok, on_aborting_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_aborting(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_aborting_result}).
%%-----------------------------------------------------------------------------
on_aborting(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_aborting ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_aborting),
    {ok, on_aborting_result}.


%%-----------------------------------------------------------------------------
%% @doc RTC behaviour callback: on_error
%% @spec on_error(RTC, PM, EC, Data) -> {ok, on_error_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_error(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_error_result}).
%%-----------------------------------------------------------------------------
on_error(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_error ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_error),
    {ok, on_error_result}.


%%-----------------------------------------------------------------------------
%% @doc RTC behaviour callback: on_reset
%% @spec on_reset(RTC, PM, EC, Data) -> {ok, on_reset_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_reset(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_reset_result}).
%%-----------------------------------------------------------------------------
on_reset(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_reset ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_reset),
    {ok, on_reset_result}.


%%-----------------------------------------------------------------------------
%% @doc DataFlowComponentAction callback: on_execute
%% @spec on_execute(RTC, PM, EC, Data) -> {ok, on_execute_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_execute(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_execute_result}).
%%-----------------------------------------------------------------------------
on_execute(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_execute ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_execute),
    {ok, on_execute_result}.


%%-----------------------------------------------------------------------------
%% @doc DataFlowComponentAction callback: on_state_update
%% @spec on_state_update(RTC, PM, EC, Data) -> {ok, on_state_update_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_state_update(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_state_update_result}).
%%-----------------------------------------------------------------------------
on_state_update(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_state_update ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_state_update),
    {ok, on_state_update_result}.


%%-----------------------------------------------------------------------------
%% @doc DataFlowComponentAction callback: on_rate_changed
%% @spec on_rate_changed(RTC, PM, EC, Data) -> {ok, on_rate_changed_result}
%% where
%%      RTC = pid()
%%      EC = ec_handle()
%%      PM = any()
%%      Data = any()
%% @end
-spec(on_rate_changed(RTC::pid(), PM::any(), EC::ec_handle(), Data::any()) ->
    {ok, on_rate_changed_result}).
%%-----------------------------------------------------------------------------
on_rate_changed(RTC, PM, EC, Data) ->
    ?LOG(rtl_paranoid, "Mock RTC behaviour in on_rate_changed ~p ~p ~p ~p",
        [RTC, PM, EC, Data]),
    gen_server:cast(mock_rtc_behv_ports, on_rate_changed),
    {ok, on_rate_changed_result}.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the server.
%% @spec init([]) -> {ok, State} | {stop, Reason}
%% where
%%      State = [atom()]
%%      Reason = any()
%% @end
-spec(init([]) -> {ok, State::[atom()]} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init([]) ->
    {ok, []}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = atom()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = [atom()]
%%      Reply = ok | [atom()]
%%      NewState = []
%% @end
-spec(handle_call(Request::atom(), From::{pid(), Tag::any()}, State::any()) ->
    {reply, Reply::ok | [atom()], NewState::[atom()]}).
%%-----------------------------------------------------------------------------
handle_call(cbs, _From, State) ->
    {reply, State, []}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) ->
%%      {noreply, NewState} | {stop, normal, State}
%% where
%%      Request = stop
%%      State = [atom()]
%% @end
-spec(handle_cast(Request::stop, State::[atom()]) ->
    {noreply, State::[atom()]} | {stop, normal, State::[atom()]}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(on_initialize, State) ->
    {noreply, [on_initialize | State]};
handle_cast(on_finalize, State) ->
    {noreply, [on_finalize | State]};
handle_cast(on_startup, State) ->
    {noreply, [on_startup | State]};
handle_cast(on_shutdown, State) ->
    {noreply, [on_shutdown | State]};
handle_cast(on_activated, State) ->
    {noreply, [on_activated | State]};
handle_cast(on_deactivated, State) ->
    {noreply, [on_deactivated | State]};
handle_cast(on_aborting, State) ->
    {noreply, [on_aborting | State]};
handle_cast(on_error, State) ->
    {noreply, [on_error | State]};
handle_cast(on_reset, State) ->
    {noreply, [on_reset | State]};
handle_cast(on_execute, State) ->
    {noreply, [on_execute | State]};
handle_cast(on_state_update, State) ->
    {noreply, [on_state_update | State]};
handle_cast(on_rate_changed, State) ->
    {noreply, [on_rate_changed | State]}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = not_supported
%%      State = [atom()]
%%      NewState = [atom()]
%% @end
-spec(handle_info(Info::not_supported, State::[atom()]) ->
    {noreply, State::[atom()]}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = [atom()]
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::[atom()]) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, _State) ->
    ?LOG(rtl_info, "Mock RTC behaviour ~p shutting down normally.", [self()]);
terminate(shutdown, _State) ->
    ?LOG(rtl_info, "Mock RTC behaviour ~p shut down by supervisor.", [self()]);
terminate({shutdown, Reason}, _State) ->
    ?LOG(rtl_info, "Mock RTC behaviour ~p shutting down: ~p",
        [self(), Reason]);
terminate(Reason, _State) ->
    ?LOG(rtl_info, "Mock RTC behaviour ~p unusual shutdown: ~p",
        [self(), Reason]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = [atom()]
%%      Extra = any()
%%      NewState = [atom()]
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::[atom()],
        Extra::any()) -> {ok, NewState::[atom()]}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

