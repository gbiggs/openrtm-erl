==============
OpenRTM-erlang
==============

--------------------
A concurrent OpenRTM
--------------------

Introduction
============

OpenRTM-erlang is an implementation of the OMG RTC standard and
OpenRTM-aist in Erlang, a concurrent programming language.

Erlang is well-suited to two programming domains: concurrent systems and
information processing. OpenRTM-erlang allows the easy implementation of
components that specialise in these areas.

Status
======

OpenRTM-erlang is currently under development. It is not yet feature
complete compared with OpenRTM-aist. The following features are missing:

 - Service ports
 - Manager
 - Plugin loading

The RT-Component implementation itself is complete and usable.

Usage
=====

OpenRTM-erlang is an RTC implementation application. It does not
currently include a component manager, unlike OpenRTM-aist. You must
integrate RTCs into your own applications yourself. It does provide
useful facilities for helping you create RTCs, such as a configuration
file loader and a logging process implementation. See the examples
(reproduced below) for how to implement and use RTCs.

Implementing RTCs
=================

An RTC in OpenRTM-erlang is implemented as a behaviour implementation
module. This module can be called anything. It must export the state
machine callbacks used by OpenRTM-erlang. These are:

  - on_initialize/2
  - on_finalize/3
  - on_startup/4
  - on_shutdown/4
  - on_activated/4
  - on_deactivated/4
  - on_aborting/4
  - on_error/4
  - on_reset/4
  - on_execute/4
  - on_state_update/4
  - on_rate_changed/4

All callbacks receive the PID of the RTC's main process and the PID of
the RTC's port manager. These must only be used when calling the ``rtc``
module API functions for managing the component, or when managing ports.

All callbacks except ``on_initialize`` receive a ``Data`` parameter.
This is the current RTC state data. It may contain any value, and is
where you should store the current state of your RTC. As an example, the
``ticker`` sample RTC uses this to store the current value of its
counter. The initial value is provided as the return value from
``on_initialize``. For example, in ``{ok, 0}``, the RTC's state data is
initialised to 0.

All callbacks except ``on_initialize`` and ``on_finalize`` receive the
handle of the Execution Context calling the callback.

Instantiating RTCs
==================

RTCs are instantiated using the ``rtc:create/3`` function. This requires
a configuration for the RTC (an empty configuration will lead to a
passive RTC with no Execution Contexts), a function to generate the
instance name of the RTC and the module that implements the RTC's
behaviour (an atom). You must then initialise the RTC's Finite State
Machine. It can then be activated as normal. For example::

  {ok, RTC, FSM} = rtc:create([], fun num_sfx_gen/0, my_rtc_behv),
  ok = rtc:initialize(FSM)

Connecting ports
================

Ports can be connected using the ``portsvc:connect/2`` function. It
requires a connection profile (which contains the ports to connect
within it) and the PID of one of the ports involved in the connection.
For example::

  CP = #conn_prof{ports=[Port1Pid, Port2Pid], ...},
  {ok, #conn_prof{}} = portsvc:connect(Port1Pid, CP)

CORBA interface
===============

If you start CORBA in your application, any RTCs that are created will
automatically create corresponding CORBA objects and register on the
configured name server. You can manage your RTCs using these interfaces
as with any other CORBA-based OpenRTM implementation, such as
OpenRTM-aist.

Example data producer
=====================

This sample RTC shows how to write data to a port and how to manage an
RTC's internal data.

::

  %%-----------------------------------------------------------------------------
  %% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
  %% @doc Ticker RTC example.
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
  -module(ticker).


  %%-----------------------------------------------------------------------------
  %% Include files
  %%-----------------------------------------------------------------------------
  -include("type_specs.hrl").
  -include("log.hrl").
  -include("ec.hrl").
  -include("../src/idl/BasicDataType.hrl").


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
      % Add an output port for the tick values
      % The port is named "tick", uses the "TimedLong" data type, and has no
      % customised configuration ([]).
      ok = port_mgr:add_port(PM, dataout, "tick", "TimedLong", RTC, []),
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
      {MS, S, Ms} = now(),
      portsvc:write(P,
          #'RTC_TimedLong'{tm=#'RTC_Time'{sec=MS * 1000000 + S, nsec=Ms * 1000},
              data=Data}),
      % Return the return code and the new version of the RTC's state data. In
      % this callback, it is unchanged.
      {ok, Data + 1}.


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

Example data consumer
=====================

This sample RTC shows how to receive data from a port.

::

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

Example application
===================

This sample shows how to instantiate two components, connect their
ports, and activate them.

::

  %%-----------------------------------------------------------------------------
  %% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
  %% @doc Sample runner file.
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
  -module(run_sample).


  %%-----------------------------------------------------------------------------
  %% Include files
  %%-----------------------------------------------------------------------------
  -include("port.hrl").


  %%-----------------------------------------------------------------------------
  %% Macros
  %%-----------------------------------------------------------------------------


  %%-----------------------------------------------------------------------------
  %% External exports
  %%-----------------------------------------------------------------------------
  -export([run/0]).


  %%-----------------------------------------------------------------------------
  %% Internal exports
  %%-----------------------------------------------------------------------------


  %%=============================================================================
  %% External functions
  %%=============================================================================

  %%-----------------------------------------------------------------------------
  %% @doc Run the sample.
  %% @spec run() -> ok
  %% @end
  -spec(run() -> ok).
  %%-----------------------------------------------------------------------------
  run() ->
      start_orber(),
      %application:load(sasl),
      %application:start(sasl),
      % Create the components
      {ok, _Ticker, TickerFSM} = make_rtc(ticker, "Ticker"),
      {ok, _Printer, PrinterFSM} = make_rtc(printer, "Printer"),
      % Get the ports
      [OP] = rtc:get_ports(TickerFSM),
      [IP] = rtc:get_ports(PrinterFSM),
      % Make a connection
      {ok, #conn_prof{}} = portsvc:connect(OP, make_conn_prof(OP, IP)),
      % Go for a long walk on the beach
      io:get_chars("Press any key to quit.", 1),
      ok.


  %%=============================================================================
  %% Internal functions
  %%=============================================================================

  %%-----------------------------------------------------------------------------
  %% @doc Create and activate a component.
  %% @spec make_rtc(M, Type) -> ok | error
  %% where
  %%      M = any()
  %%          Behaviour module for the component.
  %%      Type = string()
  %%          Type name of the component.
  %% @end
  -spec(make_rtc(M::any(), Type::string()) -> ok).
  %%-----------------------------------------------------------------------------
  make_rtc(M, Type) ->
      % Instantiate the component
      {ok, RTC, FSM} = rtc:create(one_ec_cfg(Type), fun num_sfx_gen/0, M),
      % Initialise the component
      ok = rtc:initialize(FSM),
      % Get the execution context and use it to activate the component
      [EC] = rtc:get_owned_contexts(FSM),
      ok = ec:activate_component(EC, FSM),
      % Return
      {ok, RTC, FSM}.


  %%-----------------------------------------------------------------------------
  %% @doc Start orber if necessary.
  %% @spec start_orber() -> ok | error
  %% @end
  -spec(start_orber() -> ok).
  %%-----------------------------------------------------------------------------
  start_orber() ->
      case whereis(orber_sup)
          of undefined ->
              orber:jump_start([{iiop_port,28090}]),
              ok
           ; _ ->
              error
      end.


  %%-----------------------------------------------------------------------------
  %% @doc Create a configuration with one EC.
  %% @spec one_ec_cfg(Type) -> [{string(), any()}]
  %% where
  %%      Type = string()
  %%          Type name of the component.
  %% @end
  -spec(one_ec_cfg(Type::string()) -> [{string(), any()}]).
  %%-----------------------------------------------------------------------------
  one_ec_cfg(Type) ->
      config:set_value("exec_cxts",
          [{"ec1", [{"type", "periodic"}, {"rate", "1.0"}]}],
          simple_cfg(Type)).


  %%-----------------------------------------------------------------------------
  %% @doc Create a simple configuration.
  %% @spec simple_cfg(Type) -> [{string(), any()}]
  %% where
  %%      Type = string()
  %%          Type name of the component.
  %% @end
  -spec(simple_cfg(Type::string()) -> [{string(), any()}]).
  %%-----------------------------------------------------------------------------
  simple_cfg(Type) ->
      Cfg1 = config:set_value("implementation_id", Type,
          config:empty_conf()),
      Cfg2 = config:set_value("type_name", Type, Cfg1),
      Cfg3 = config:set_value("description", "Fake RTC.", Cfg2),
      Cfg4 = config:set_value("version", "1.0", Cfg3),
      Cfg5 = config:set_value("vendor", "Me", Cfg4),
      Cfg6 = config:set_value("category", "Test", Cfg5),
      Cfg7 = config:set_value("activity_type", "DataFlowComponent", Cfg6),
      Cfg8 = config:set_value("max_instance", "1", Cfg7),
      Cfg9 = config:set_value("language", "Erlang", Cfg8),
      Cfg10 = config:set_value("lang_type", "compile", Cfg9),
      config:set_value("corba", [{"nameservers", "127.0.0.1:28090"}], Cfg10).


  %%-----------------------------------------------------------------------------
  %% @doc Function to return a textual number.
  %% @spec num_sfx_gen() -> "0".
  %% @end
  -spec(num_sfx_gen() -> [pos_integer()]).
  %%-----------------------------------------------------------------------------
  num_sfx_gen() ->
      "0".


  %%-----------------------------------------------------------------------------
  %% @doc Make a default connection profile.
  %% @spec make_conn_prof(Port1, Port2) -> #conn_prof{}
  %% where
  %%      Port1 = any()
  %%      Port2 = any()
  %% @end
  -spec(make_conn_prof(Port1::any(), Port2::any()) -> #conn_prof{}).
  %%-----------------------------------------------------------------------------
  make_conn_prof(Port1, Port2) ->
      #conn_prof{name="inport_outport", id="", ports=[Port1, Port2],
          props=[{"dataport", [{"dataflow_type", "push"},
                  {"data_type", portsvc:get_datatype(Port1)},
                  {"interface_type", "erlang"},
                  {"subscription_type", "new"}]}]}.

