%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc RTC:PortService implementation.
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
-module('OpenRTM_InPortCdr_impl').

%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("orber/src/orber_iiop.hrl").
-include("idl/RTC.hrl").
-include("type_specs.hrl").
-include("log.hrl").

%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------
-record(inportcdr_svc, {target=nil :: pid() | nil % Target process
    }).


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/1, stop_intf/1]).
% OpenRTM:InPortCdr interface
-export([put/3]).


%%----------------------------------------------------------------------
%% Internal Exports
%%----------------------------------------------------------------------
-export([init/1, handle_info/2, terminate/2, code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the InPortCdr interface.
%% @spec start_link(none) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link(none) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link(none) ->
    'OpenRTM_InPortCdr':oe_create_link(none, [{sup_child, true}]).


%%-----------------------------------------------------------------------------
%% @doc Stop the InPortCdr interface.
%% @spec stop_intf(Pid) -> ok
%% where
%%      Pid = pid(
%%          The PID of the process running the interface.
%% @end
-spec(stop_intf(Pid::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_intf(Pid) ->
    gen_server:cast(Pid, stop).


%%-----------------------------------------------------------------------------
%% @doc Put data.
%% @spec put(This, State, Data) -> {reply, Result, NewState}
%% where
%%      This = object_ref()
%%      State = #inportcdr_svc{}
%%      Data = [octet()]
%%      Result = 'PORT_OK' | 'PORT_ERROR' | 'BUFFER_FULL' | 'BUFFER_EMPTY' |
%%          'BUFFER_TIMEOUT' | 'UNKNOWN_ERROR'
%%      NewState = #inportcdr_svc{}
%% @end
-spec(put(This::object_ref(), State::#inportcdr_svc{}, Data::any()) ->
    {reply, Result::'PORT_OK' | 'PORT_ERROR' | 'BUFFER_FULL' | 'BUFFER_EMPTY' |
        'BUFFER_TIMEOUT' | 'UNKNOWN_ERROR', NewState::#inportcdr_svc{}}).
%%-----------------------------------------------------------------------------
put(_OE_This, #inportcdr_svc{target=T}=State, Data) ->
    ?LOG(rtl_debug, "CORBA interface for ~p put data~n~p", [T, Data]),
    Reply = connection:put(T, Data),
    {reply, utils:port_rc_to_corba(Reply), State}.


%%=============================================================================
%% Internal Functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc gen_server initialisation callback function.
%% @spec init(none) -> {ok, State} | {stop, Reason}
%% where
%%      State = #port_svc{}
%%      Reason = any()
%% @end
-spec(init(none) -> {ok, State::#inportcdr_svc{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init(none) ->
    ?LOG(rtl_info, "Initialising CORBA OpenRTM:InPortCdr interface."),
    {ok, #inportcdr_svc{}}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = {set_target, Target}
%%      Target = pid()
%%      State = #inportcdr_svc{}
%%      NewState = #inportcdr_svc{}
%% @end
-spec(handle_info(Info::{set_target, Target::pid()}, State::#inportcdr_svc{}) ->
    {noreply, NewState::#inportcdr_svc{}}).
%%-----------------------------------------------------------------------------
handle_info({set_target, Target}, State) ->
    ?LOG(rtl_paranoid, "Received put_listener target: ~p", [Target]),
    {noreply, State#inportcdr_svc{target=Target}}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any} | any()
%%      State = #inportcdr_svc{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#inportcdr_svc{}) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, _State) ->
    ?LOG(rtl_info, "Shutting down normally.");
terminate(shutdown, _State) ->
    ?LOG(rtl_info, "Shut down by supervisor.");
terminate({shutdown, Reason}, _State) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]);
terminate(Reason, _State) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = #inportcdr_svc{}
%%      Extra = any()
%%      NewState = #inportcdr_svc{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#inportcdr_svc{},
        Extra::any()) -> {ok, NewState::#inportcdr_svc{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

