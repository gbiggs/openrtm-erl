%%----------------------------------------------------------------------
%% <LICENSE>
%% 
%%     $Id$
%%
%%----------------------------------------------------------------------
%% Module       : OpenRTM_OutPortCdr_impl.erl
%% 
%% Source       : /home/geoff/research/src/local/openrtm-erl/src/idl/DataPort.idl
%% 
%% Description  : 
%% 
%% Creation date: 2010-12-15
%%
%%----------------------------------------------------------------------
-module('OpenRTM_OutPortCdr_impl').

-export([get/2]).

%%----------------------------------------------------------------------
%% Internal Exports
%%----------------------------------------------------------------------
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2]).

%%----------------------------------------------------------------------
%% Include Files
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {}).

%%======================================================================
%% API Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : get/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = {OE_Reply, Data}
%%              OE_Reply = PortStatus
%%              PortStatus = 'PORT_OK' | 'PORT_ERROR' | 'BUFFER_FULL' | 'BUFFER_EMPTY' | 'BUFFER_TIMEOUT' | 'UNKNOWN_ERROR' 
%%              Data = [ DataElem ]
%%              DataElem = octet()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get(OE_This, State) ->
	{reply, {OE_Reply, Data}, State}.

%%======================================================================
%% Internal Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Arguments  : Env = term()
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Raises     : -
%% Description: Initiates the server
%%----------------------------------------------------------------------
init(_Env) ->
	{ok, #state{}}.


%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Arguments  : Reason = normal | shutdown | term()
%%              State = term()
%% Returns    : ok
%% Raises     : -
%% Description: Invoked when the object is terminating.
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.


%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Arguments  : OldVsn = undefined | term()
%%              State = NewState = term()
%%              Extra = term()
%% Returns    : {ok, NewState}
%% Raises     : -
%% Description: Invoked when the object should update its internal state
%%              due to code replacement.
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%----------------------------------------------------------------------
%% Function   : handle_info/2
%% Arguments  : Info = normal | shutdown | term()
%%              State = NewState = term()
%% Returns    : {noreply, NewState}          |
%%              {noreply, NewState, Timeout} |
%%              {stop, Reason, NewState}
%% Raises     : -
%% Description: Invoked when, for example, the server traps exits.
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.


