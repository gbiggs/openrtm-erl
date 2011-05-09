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
    %{ok, _Printer, PrinterFSM} = make_rtc(printer, "Printer"),
    % Get the ports
    %[OP] = rtc:get_ports(TickerFSM),
    %[IP] = rtc:get_ports(PrinterFSM),
    % Make a connection
    %{ok, #conn_prof{}} = portsvc:connect(OP, make_conn_prof(OP, IP)),
    % Go for a long walk on the beach
    io:get_chars("Press any key to quit.", 1),
    stop_rtc(TickerFSM),
    %stop_rtc(PrinterFSM),
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
    ok = ec:start(EC),
    ok = ec:activate_component(EC, FSM),
    % Return
    {ok, RTC, FSM}.


%%-----------------------------------------------------------------------------
%% @doc Deactivate and destroy a component.
%% @spec stop_rtc(RTC) -> ok | error
%% where
%%      RTC = pid()
%% @end
-spec(stop_rtc(RTC::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_rtc(RTC) ->
    % Get the execution context and use it to deactivate the component
    [EC] = rtc:get_owned_contexts(RTC),
    ok = ec:deactivate_component(EC, RTC),
    ok = ec:stop(EC),
    % Destroy the component
    rtc:destroy(RTC),
    % Return
    ok.


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


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

