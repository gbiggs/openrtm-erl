%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Record definitions for the manager.
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

-ifndef(mgr_hrl__).
-define(mgr_hrl__(), ok).

%%=============================================================================
%% Include files
%%=============================================================================
-include("ec.hrl").
-include("rtc.hrl").


%%=============================================================================
%% Types
%%=============================================================================
-record(mgr, {pid::pid(), % PID of this manager.
        mgrs=[]::[#mgr{}], % List of child managers.
        parent=none::pid() | none % The PID of this manager's parent.
    }
).


%%=============================================================================
%% Macros
%%=============================================================================

-endif. % mgr_hrl__

