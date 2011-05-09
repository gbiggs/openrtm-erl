%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Random utility functions.
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
-module(utils).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([str_to_float/1, to_rtc_rc/1, from_rtc_rc/1, port_rc_to_corba/1,
    port_rc_from_corba/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Converts a string representation of a number to a float, ignoring any
%% trailing characters.
%% @spec str_to_float(string()) -> float() | {error, no_number}
%% @end
-spec(str_to_float(S::string()) -> float()).
%%-----------------------------------------------------------------------------
str_to_float(S) when is_list(S) ->
    case string:to_float(S)
        of {error, no_float} ->
            case string:to_integer(S)
                of {error, no_integer} ->
                    {error, no_number}
                 ; {I, _} ->
                    float(I)
            end
         ; {F, _} ->
            F
    end.


%%-----------------------------------------------------------------------------
%% @doc Converts a local return code into an RTC return code.
%% @spec to_rtc_rc(rtc_cb_return()) -> rtc_returncode()
%% @end
-spec(to_rtc_rc(rtc_cb_return()) -> rtc_returncode()).
%%-----------------------------------------------------------------------------
to_rtc_rc(ok) ->
    'RTC_OK';
to_rtc_rc(error) ->
    'RTC_ERROR';
to_rtc_rc(bad_param) ->
    'BAD_PARAMETER';
to_rtc_rc(unsupported) ->
    'UNSUPPORTED';
to_rtc_rc(out_of_res) ->
    'OUT_OF_RESOURCES';
to_rtc_rc(precon_not_met) ->
    'PRECONDITION_NOT_MET'.


%%-----------------------------------------------------------------------------
%% @doc Converts an RTC return code into a local return code.
%% @spec to_rtc_rc(rtc_cb_return()) -> rtc_returncode()
%% @end
-spec(from_rtc_rc(rtc_returncode()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
from_rtc_rc('RTC_OK') ->
    ok;
from_rtc_rc('RTC_ERROR') ->
    error;
from_rtc_rc('BAD_PARAMETER') ->
    bad_param;
from_rtc_rc('UNSUPPORTED') ->
    unsupported;
from_rtc_rc('OUT_OF_RESOURCES') ->
    out_of_res;
from_rtc_rc('PRECONDITION_NOT_MET') ->
    precon_not_met.


%%-----------------------------------------------------------------------------
%% @doc Convert a put return code from CORBA
%% @spec port_rc_from_corba(RC) -> C_Reply
%% where
%%      RC = 'PORT_OK' | 'PORT_ERROR' | 'BUFFER_FULL' | 'BUFFER_EMPTY' |
%%          'BUFFER_TIMEOUT' | 'UNKNOWN_ERROR'
%%      Reply = port_ok | port_err | buff_full | buff_empty | buff_timeout |
%%          unknown_err
%% @end
-spec(port_rc_from_corba('PORT_OK' | 'PORT_ERROR' | 'BUFFER_FULL' |
        'BUFFER_EMPTY' | 'BUFFER_TIMEOUT' | 'UNKNOWN_ERROR') ->
    port_ok | port_err | buff_full | buff_empty | buff_timeout | unknown_err).
%%-----------------------------------------------------------------------------
port_rc_from_corba('PORT_OK') ->
    port_ok;
port_rc_from_corba('PORT_ERROR') ->
    port_err;
port_rc_from_corba('BUFFER_FULL') ->
    buff_full;
port_rc_from_corba('BUFFER_EMPTY') ->
    buff_empty;
port_rc_from_corba('BUFFER_TIMEOUT') ->
    buff_timeout;
port_rc_from_corba('UNKNOWN_ERROR') ->
    unknown_err.


%%-----------------------------------------------------------------------------
%% @doc Convert a put reply to CORBA
%% @spec port_rc_to_corba(Reply) -> C_Reply
%% where
%%      Reply = port_ok | port_err | buff_full | buff_empty | buff_timeout |
%%          unknown_err
%%      C_Reply = 'PORT_OK' | 'PORT_ERROR' | 'BUFFER_FULL' | 'BUFFER_EMPTY' |
%%          'BUFFER_TIMEOUT' | 'UNKNOWN_ERROR'
%% @end
-spec(port_rc_to_corba(port_ok | port_err | buff_full | buff_empty |
        buff_timeout | unknown_err) ->
    'PORT_OK' | 'PORT_ERROR' | 'BUFFER_FULL' | 'BUFFER_EMPTY' |
        'BUFFER_TIMEOUT' | 'UNKNOWN_ERROR').
%%-----------------------------------------------------------------------------
port_rc_to_corba(port_ok) ->
    'PORT_OK';
port_rc_to_corba(port_err) ->
    'PORT_ERROR';
port_rc_to_corba(buff_full) ->
    'BUFFER_FULL';
port_rc_to_corba(buff_empty) ->
    'BUFFER_EMPTY';
port_rc_to_corba(buff_timeout) ->
    'BUFFER_TIMEOUT';
port_rc_to_corba(unknown_err) ->
    'UNKNOWN_ERROR'.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test converting strings to floats.
%% @spec str_to_float_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(str_to_float_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
str_to_float_test_() ->
    [?_assertMatch(0.0, str_to_float("0")),
        ?_assertMatch(1.0, str_to_float("1")),
        ?_assertMatch(-1.0, str_to_float("-1")),
        ?_assertMatch(0.0, str_to_float("0.0")),
        ?_assertMatch(1.0, str_to_float("1.0")),
        ?_assertMatch(-1.0, str_to_float("-1.0")),
        ?_assertMatch({error, no_number}, str_to_float("")),
        ?_assertMatch({error, no_number}, str_to_float("word")),
        ?_assertMatch({error, no_number}, str_to_float("one point five"))].

-endif. % TEST

