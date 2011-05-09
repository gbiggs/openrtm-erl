%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Server providing access to a buffer. The specific buffer type is
%% determined by the buffer module provided at initialisation.
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
-module(buffer_svr).
-behaviour(gen_server).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Server control
-export([start_link/2, stop_server/1]).
% Buffer functionality
-export([write/2, read/1, count/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2,
    code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the server.
%% @spec start_link(BufferMod, BufferConfig) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      BufferMod = atom()
%%      BufferConfig = proplist()
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link(BufferMod::atom(), BufferConfig::proplist()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link(BufferMod, BufferConfig) ->
    gen_server:start_link(buffer_svr, {BufferMod, BufferConfig}, []).


%%-----------------------------------------------------------------------------
%% @doc Stop the server.
%% @spec stop_server(Svr) -> ok
%% where
%%      Svr = pid()
%% @end
-spec(stop_server(Svr::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_server(Svr) ->
    gen_server:cast(Svr, stop).


%%-----------------------------------------------------------------------------
%% @doc Write a value to the buffer.
%% @spec write(Svr, Value) -> ok | overflow | {error, Reason}
%% where
%%      Svr = pid()
%%      Value = any()
%%      Reason = any()
%% @end
-spec(write(Svr::pid(), Value::any()) ->
    ok | overflow | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
write(Svr, Value) ->
    gen_server:call(Svr, {write, Value}).


%%-----------------------------------------------------------------------------
%% @doc Read the top value from the buffer.
%% @spec read(Svr) -> Value | {error, empty | Reason}
%% where
%%      Svr = pid()
%%      Value = any()
%%      Reason = any()
%% @end
-spec(read(Svr::pid()) -> Value::any() | {error, empty | any()}).
%%-----------------------------------------------------------------------------
read(Svr) ->
    gen_server:call(Svr, read).


%%-----------------------------------------------------------------------------
%% @doc Count the number of items in the buffer.
%% @spec count(Svr) -> 0 | pos_integer()
%% where
%%      Svr = pid()
%% @end
-spec(count(Svr::pid()) -> 0 | pos_integer()).
%%-----------------------------------------------------------------------------
count(Svr) ->
    gen_server:call(Svr, count).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the server.
%% @spec init({BufferMod, BufferConfig}) -> {ok, State} | {stop, Reason}
%% where
%%      BufferMod = atom()
%%      BufferConfig = proplist()
%%      State = {atom(), any()}
%%      Reason = any()
%% @end
-spec(init(BufferMod::atom()) ->
    {ok, State::{atom(), any()}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init({BufferMod, BufferConfig}) ->
    process_flag(trap_exit, true),
    ?LOG(rtl_debug, "Buffer server starting: ~p ~p",
        [BufferMod, BufferConfig]),
    {ok, {BufferMod, apply(BufferMod, new, [BufferConfig])}}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = any()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = {atom(), any()}
%%      Reply = any()
%%      NewState = {atom(), any()}
%% @end
-spec(handle_call(Request::any(), From::{pid(), Tag::any()},
        State::{atom(), any()}) ->
    {reply, Reply::any(), NewState::{atom(), any()}}).
%%-----------------------------------------------------------------------------
handle_call({write, Value}, _From, {BufMod, Buf}) ->
    {Result, NewBuf} = apply(BufMod, write, [Value, Buf]),
    {reply, Result, {BufMod, NewBuf}};
handle_call(read, _From, {BufMod, Buf}) ->
    {Result, NewBuf} = apply(BufMod, read, [Buf]),
    {reply, Result, {BufMod, NewBuf}};
handle_call(count, _From, {BufMod, Buf}) ->
    {Result, NewBuf} = apply(BufMod, count, [Buf]),
    {reply, Result, {BufMod, NewBuf}}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(stop, State) -> {stop, normal, NewState}
%% where
%%      State = {atom(), any()}
%%      NewState = {atom(), any()}
%% @end
-spec(handle_cast(Request::stop, State::{atom(), any()}) ->
    {stop, normal, NewState::{atom(), any()}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(not_supported, State) -> {noreply, NewState}
%% where
%%      Info = not_supported
%%      State = {atom(), any()}
%%      NewState = {atom(), any()}
%% @end
-spec(handle_info(Info::not_supported, State::{atom(), any()}) ->
    {noreply, NewState::{atom(), any()}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = {atom(), any()}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::{atom(), any()}) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, {BufMod, Buf}) ->
    ?LOG(rtl_info, "Buffer server shutting down normally."),
    apply(BufMod, delete, [Buf]);
terminate(shutdown, {BufMod, Buf}) ->
    ?LOG(rtl_info, "Buffer server shut down by supervisor."),
    apply(BufMod, delete, [Buf]);
terminate({shutdown, Reason}, {BufMod, Buf}) ->
    ?LOG(rtl_info, "Buffer server shut down by supervisor: ~p", [Reason]),
    apply(BufMod, delete, [Buf]);
terminate(Reason, {BufMod, Buf}) ->
    ?LOG(rtl_info, "Buffer server shut down unusually: ~p", [Reason]),
    apply(BufMod, delete, [Buf]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = {atom(), any()}
%%      Extra = any()
%%      NewState = {atom(), any()}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::{atom(), any()},
        Extra::any()) -> {ok, NewState::{atom(), any()}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

