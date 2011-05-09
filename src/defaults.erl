%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Hard-coded default values for the configuration file server.
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
-module(defaults).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include("type_specs.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([defaults/0]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Provides the default configuration parameters.
%% @spec defaults() -> proplist()
%% @end
-spec(defaults() -> proplist()).
%%-----------------------------------------------------------------------------
defaults() ->
    [
        {"logger",
            [{"enable", true},
            {"file", [
                {"name", "rtc%p.log"},
                {"level", "rtl_paranoid"}
            ]},
            {"terminal", [
                {"level", "rtl_paranoid"}
            ]},
            {"outputs", "terminal, file"}
        ]},
        {"manager", [
            {"modules", [
                {"abs_path_allowed", true},
                {"load_path", ""},
                {"preload", ""}
            ]},
            {"components", [
                {"precreate", ""}
            ]},
            {"name", "manager"},
            {"naming_formats", "%h.host/%n.mgr"},
            {"shutdown_on_nortcs", true},
            {"shutdown_auto", true}
        ]},
        {"naming", [
            {"enable", true},
            {"type", "erlang,corba"},
            {"formats", "%h.host/%n.rtc"}
        ]},
        {"corba", [
            {"nameservers", "localhost"}
        ]},
        {"exec_cxt", [
            {"periodic", [
                {"type", "periodic_ec"},
                {"rate", "1000.0"}
            ]},
            {"evdriven", [
                {"type", "evdriven_ec"}
            ]}
        ]}
    ].


%%=============================================================================
%% Internal functions
%%=============================================================================


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

