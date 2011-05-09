%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Logging macros.
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

-ifndef(log_hrl__).
-define(log_hrl__(), ok).

%%=============================================================================
%% Types
%%=============================================================================
%% @type log_level() = rtl_silent | rtl_fatal | rtl_error | rtl_warn |
%%      rtl_info | rtl_debug | rtl_trace | rtl_verbose | rtl_paranoid.
%%  A threshold level to filter logged events.
-type(log_level() :: rtl_silent | rtl_fatal | rtl_error | rtl_warn | rtl_info |
    rtl_debug | rtl_trace | rtl_verbose | rtl_paranoid).


%%=============================================================================
%% Include files
%%=============================================================================


%%=============================================================================
%% Macros
%%=============================================================================
-ifdef(NOLOGGING).
-define(LOG(Level, Fmt), ok).
-define(LOG(Level, Fmt, Vals), ok).
-else.
-define(LOG(Level, Fmt),
    log_mgr:log(Level, {?MODULE_STRING, ?LINE, self()}, Fmt)).
-define(LOG(Level, Fmt, Vals),
    log_mgr:log(Level, {?MODULE_STRING, ?LINE, self()}, Fmt, Vals)).
-endif. % NOLOGGING

-endif. % log_hrl__

