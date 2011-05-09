%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Configuration server.
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
-module(config_svr).
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
-export([start_link/2, stop/0]).
% Services
-export([get_value/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the configuration server.
%% @spec start_link(Filename, Defaults) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Filename = string()
%%      Defaults = config()
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link(Filename::string(), Defaults::config()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link(Filename, Defaults) ->
    gen_server:start_link({local, config_svr}, config_svr,
        {Filename, Defaults}, []).


%%-----------------------------------------------------------------------------
%% @doc Stop the configuration server.
%% @spec stop() -> ok
%% @end
-spec(stop() -> ok).
%%-----------------------------------------------------------------------------
stop() ->
    gen_server:cast(config_svr, stop).


%%-----------------------------------------------------------------------------
%% @doc Get a value from the server.
%% @spec get_value(Key) -> string() | undefined
%% where
%%      Key = string() | [string()]
%% @end
-spec(get_value(Key::string() | [string()]) -> string() | undefined).
%%-----------------------------------------------------------------------------
get_value(Key) ->
    gen_server:call(config_svr, {get_value, Key}).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the configuration file server. Loads the specified
%% configuration file, if possible. After loading a file, the defaults merged
%% in. Any values not specified in the configuration will be filled in from the
%% defaults, the rest will be left untouched.
%% If FileName is empty, the defaults only will be used.
%% @spec init({FileName, Defaults}) -> {ok, Config} | {stop, Reason}
%% where
%%      FileName = string()
%%      Defaults = config()
%%      Config = config()
%%      Reason = any()
%% @end
-spec(init({FileName::string(), Defaults::config()}) ->
        {ok, config()} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init({"", Defaults}) ->
    ?LOG(rtl_info, "Initialising the configuration file server."),
    process_flag(trap_exit, true),
    ?LOG(rtl_info, "No configuration file to load. Using defaults."),
    ?LOG(rtl_debug, "Config server initialised state:"),
    ?LOG(rtl_debug, "~p", [Defaults]),
    {ok, Defaults};
init({FileName, Defaults}) ->
    ?LOG(rtl_info, "Initialising the configuration file server."),
    process_flag(trap_exit, true),
    Result = case file:open(FileName, [read])
        of {ok, Fd} ->
            {ok, C} = config:read_conf_file(Fd),
            config:merge(Defaults, C)
         ; {error, enoent} ->
            ?LOG(rtl_warn, io_lib:format("Failed to open ~s, using defaults",
                    [FileName])),
            Defaults
    end,
    ?LOG(rtl_debug, "Config server initialised state:"),
    ?LOG(rtl_debug, "~p", [Result]),
    {ok, Result}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = any()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = config()
%%      Reply = any()
%%      NewState = config()
%% @end
-spec(handle_call(Request::any(), From::{pid(), Tag::any()},
        State::config()) ->
    {reply, Reply::any(), NewState::config()}).
%%-----------------------------------------------------------------------------
handle_call({get_value, K}, _From, State) ->
    {reply, config:get_value(K, State), State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) -> {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = config()
%%      NewState = config()
%% @end
-spec(handle_cast(Request::any(), State::config()) ->
    {stop, normal, NewState::config()}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {ok, NewState}
%% where
%%      Info = timeout | any()
%%      State = config()
%%      NewState = config()
%% @end
-spec(handle_info(Info::timeout | any(), State::config()) ->
    {ok, NewState::config()}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = config()
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::config()) -> ok).
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
%%      State = config()
%%      Extra = any()
%%      NewState = config()
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::config(),
        Extra::any()) -> {ok, NewState::config()}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

