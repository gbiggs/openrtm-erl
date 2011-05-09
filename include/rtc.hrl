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

-ifndef(rtc_hrl__).
-define(rtc_hrl__(), ok).

%%=============================================================================
%% Include files
%%=============================================================================
-include("port.hrl").
-include("type_specs.hrl").
-include("ec.hrl").


%%=============================================================================
%% Types
%%=============================================================================
-type(rtc_lifecyclestate() :: inactive | active | error).

-record(df_rtc, {cfg_svr=nil :: pid() | nil, % Configuration parameters manager
        cfg=[] :: config(), % Configuration of the RTC
        port_mgr=nil :: pid() | nil, % Ports manager
        ecs_sup=nil :: pid() | nil, % Execution contexts supervisor
        owned_ecs=[] :: [{ec_handle(), Sup::pid(), Worker::pid(), ID::string()}], % Owned ECs
        part_ecs=[] :: [{ec_handle(), pid()}], % Participating ECs
        next_ech=0 :: 0 | pos_integer(), % The next EC handle to use
        sup_pid=nil :: pid() | nil, % The PID of the supervisor for the RTC
        helpers=[] :: [{Helper::pid(), Caller::pid()}], % List of helper processes
        cbs=[] :: [{Ref::reference(), Event::atom(), CB::fun()}], % Callbacks
        mod :: atom(), % Callback module
        data=none :: any() % Data of the callback module
    }).

-record(comp_prof, {inst_name :: string(), % Unique instance name
        type_name :: string(), % Type of the component
        desc :: string(), % Description for humans
        ver :: string(), % Version of the component
        vendor :: string(), % Name of the creator
        category :: string(), % Group in which the component belongs
        port_profs :: [#port_prof{}], % Profiles of the component's ports
        parent :: pid() | nil, % Parent if this component is in a composite, or nil otherwise
        props=[] :: proplist() % Implementation-specific properties
    }).


%%=============================================================================
%% Macros
%%=============================================================================

-endif. % rtc_hrl__

