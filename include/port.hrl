%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Record definitions for RT Components.
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

-ifndef(port_hrl__).
-define(port_hrl__(), ok).

%%=============================================================================
%% Include files
%%=============================================================================
-include("type_specs.hrl").


%%=============================================================================
%% Types
%%=============================================================================
-type(port_type() :: datain | dataout | svc).
-type(port_intf_polarity() :: provided | required).
-record(port_intf_prof, {inst_name :: string(), % Name of the interface instance
        type_name :: string(), % Type of the interface
        polarity :: port_intf_polarity() % Whether the interface is provided or required
    }).

-type(id() :: string()).
-record(conn_prof, {name :: string(), % Name of the connection
        id :: id(), % Unique identifier assigned to the connection
        ports :: [pid() | object_ref()], % References to the ports involved in the connection
        props=[] :: config() % Implementation-specific properties
    }).

-record(port_prof, {name :: string(), % Name of the port, unique within an RTC
        interfaces :: [#port_intf_prof{}], % Interfaces exposed by the port
        port_ref :: pid(), % The port object
        conn_profs :: [#conn_prof{}], % Connections to the port
        owner :: pid(), % The RTC that owns the port
        props=[] :: config() % Implementation-specific properties
    }).


%%=============================================================================
%% Macros
%%=============================================================================

-endif. % port_hrl__

