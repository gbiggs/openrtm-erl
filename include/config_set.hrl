%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Record definitions for configuration sets.
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

-ifndef(config_set_hrl__).
-define(config_set_hrl__(), ok).

%%=============================================================================
%% Include files
%%=============================================================================


%%=============================================================================
%% Types
%%=============================================================================
-type(config_set() :: [{string(), string()}]).
-type(config_sets() :: [{string(), config_set()}]).
-record(cfg_sets, {sets=[{"default", []}] :: config_sets(), % Configuration sets
        act=[] :: [{string(), string()}], % Currently-active set
        act_name="default" :: string() % Current-active set name
    }).


%%=============================================================================
%% Macros
%%=============================================================================

-endif. % config_set_hrl__

