%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Record definitions for execution contexts.
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

-ifndef(ec_hrl__).
-define(ec_hrl__(), ok).

%%=============================================================================
%% Include files
%%=============================================================================
-include("type_specs.hrl").


%%=============================================================================
%% Types
%%=============================================================================
-type(ec_kind() :: periodic | event_driven | other).
-type(ec_handle() :: pos_integer()).
-type(ec_ref() :: pid()).

-record(periodic_ec, {
        config=[] :: config(), % The EC's configuration.
        kind=periodic :: ec_kind(), % Kind of execution context.
        rate :: float(), % Frequency of execution, in Hertz.
        period :: pos_integer(), % The period in ms, calculated from the rate.
        owner=nil :: pid(), % The owner of this execution context, an RTC.
        parts=[] :: [{Handle::ec_handle(), LifeMgr::pid(), RTC::pid()}], % The participating RTCs.
        ticker=undefined :: reference(), % Reference to the ticker.
        c_pid=nil :: pid() | nil, % CORBA interface PID.
        c_obj=nil :: object_ref() | nil % CORBA interface object.
    }).

-record(ec_profile, {
        kind :: ec_kind(), % Kind of execution context.
        rate :: float(), % Frequency of execution, in Hertz.
        owner :: pid(), % Owning RT Component.
        parts :: [pid() | object_ref()], % Participating RT Components.
        props :: config() % Implementation-specific properties.
    }).


%%=============================================================================
%% Macros
%%=============================================================================

-endif. % ec_hrl__

