%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Printer RTC example.
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
-module(printer).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include("type_specs.hrl").
-include("log.hrl").
-include("ec.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% RTC behaviour callbacks
-export([on_initialize/2, on_finalize/3, on_startup/4, on_shutdown/4,
        on_activated/4, on_deactivated/4, on_aborting/4, on_error/4,
        on_reset/4]).
% Data flow component action callbacks
-export([on_execute/4, on_state_update/4, on_rate_changed/4]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------


%%=============================================================================
%% External functions
%%=============================================================================

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
    ?LOG(rtl_info, "Ticker example RTC on_initialize"),
    % Add an input port for values to print
    % The port is named "value", uses the "TimedLong" data type, and has no
    % customised configuration ([]).
    ok = port_mgr:add_port(PM, datain, "value", "TimedLong", RTC, []),
    % Return a return code (in this case, "ok") and the initial value for the
    % RTC's state data (in this case, the initial tick value).
    {ok, 0}.


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
on_finalize(_RTC, _PM, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_finalize"),
    % Nothing to do. The callback functions do not need to be implemented if
    % they do not have anything to do. These are included only as examples.
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_startup(_RTC, _PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_startup"),
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_shutdown(_RTC, _PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_shutdown"),
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_activated(_RTC, _PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_activated"),
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_deactivated(_RTC, _PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_deactivated"),
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_aborting(_RTC, _PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_aborting"),
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_error(_RTC, _PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_error"),
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_reset(_RTC, _PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_reset"),
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_execute(_RTC, PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_execute"),
    P = port_mgr:get_port(PM, "value"),
    case portsvc:is_new(P)
        of true ->
            {ok, V} = portsvc:read(P),
            io:format("Received value: ~p", [V])
         ; false ->
            ok
    end,
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_state_update(_RTC, _PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_state_update"),
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


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
on_rate_changed(_RTC, _PM, _EC, Data) ->
    ?LOG(rtl_info, "Ticker example RTC on_rate_changed"),
    % Return the return code and the new version of the RTC's state data. In
    % this callback, it is unchanged.
    {ok, Data}.


%%=============================================================================
%% Internal functions
%%=============================================================================


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

