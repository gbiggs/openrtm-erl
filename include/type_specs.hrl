%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc 
%% @reference <a href=""></a>
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

-ifndef(type_specs_hrl__).
-define(type_specs_hrl__(), ok).

%%=============================================================================
%% Types
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Useful miscellaneous types
%%-----------------------------------------------------------------------------
-type(proplist() :: [{any(), any()} | atom()]).


%%-----------------------------------------------------------------------------
%% Configuration types
%%-----------------------------------------------------------------------------
-type(configitem() :: {string(), string() | config()}).
-type(config() :: [configitem()]).


%%-----------------------------------------------------------------------------
%% Loadable module types
%%-----------------------------------------------------------------------------
-type(loadable_module() :: {Name::string(), module()}).
-type(mods_list() :: [loadable_module()]).


%%-----------------------------------------------------------------------------
%% Types from the IDL
%%-----------------------------------------------------------------------------
-type(object_ref() :: any()).
% ReturnCode_t
-type(rtc_returncode() :: 'RTC_OK' | 'RTC_ERROR' | 'BAD_PARAMETER' |
    'UNSUPPORTED' | 'OUT_OF_RESOURCES' | 'PRECONDITION_NOT_MET').
-type(rtc_cb_return() :: ok | error | bad_param | unsupported |
    out_of_res | precon_not_met).
% LyfeCycleState
-type(lifecyclestate() :: 'CREATED_STATE' | 'INACTIVE_STATE' | 'ACTIVE_STATE' |
    'ERROR_STATE').
% ExecutionKind
-type(executionkind() :: 'PERIODIC' | 'EVENT_DRIVEN' | 'OTHER').


%%-----------------------------------------------------------------------------
%% Supervisor types
%%-----------------------------------------------------------------------------
-type(restart_strategy() :: one_for_all | one_for_one | rest_for_one |
    simple_one_for_one).
-type(child_restart_spec() :: permanent | transient | temporary).
-type(child_shutdown_spec() :: brutal_kill | pos_integer() | infinity).
-type(child_type() :: worker | supervisor).
-type(child_modules() :: [atom()] | dynamic).
-type(child_spec() :: {Id::any(), StartFunc::{atom(), atom(), [any()]},
        Restart::child_restart_spec(), Shutdown::child_shutdown_spec(),
        Type::child_type(), Modules::child_modules()}).
-type(sup_init_res() :: {ok,
        {{restart_strategy(), MaxR::0 | pos_integer(),
                MaxT::0 | pos_integer()},
            [child_spec()]}} | ignore).

%%=============================================================================
%% Include files
%%=============================================================================


%%=============================================================================
%% Macros
%%=============================================================================

-endif. % type_specs_hrl__

