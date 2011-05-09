%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc RT Component configuration parameters server.
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
-module(rtc_cfg_svr).
-behaviour(gen_server).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("config_set.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/1, stop/1]).
% Services
-export([get_sets/1, get_active_set/1, set_active_set/2, rem_set/2]).
-export([get_values/2, set_values/3]).
-export([get_value/2, get_value/3, set_value/3, set_value/4]).
-export([has_set/2, has_key/3]).


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
%% @spec start_link(Config) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Config = config()
%%          Configuration sets and other configuration-related parameters.
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link(Config::config()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link(rtc_cfg_svr, Config, []).


%%-----------------------------------------------------------------------------
%% @doc Stop the configuration server.
%% @spec stop(Svr) -> ok
%% where
%%      Svr = pid()
%% @end
-spec(stop(Svr::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop(Svr) ->
    gen_server:cast(Svr, stop).


%%-----------------------------------------------------------------------------
%% @doc Get the available configuration sets.
%% @spec get_sets(Svr) -> Sets
%% where
%%      Svr = pid()
%%      Sets = [string()]
%% @end
-spec(get_sets(Svr::pid()) -> Sets::[string()]).
%%-----------------------------------------------------------------------------
get_sets(Svr) ->
    gen_server:call(Svr, get_sets).


%%-----------------------------------------------------------------------------
%% @doc Get the active set.
%% @spec get_active_set(Svr) -> SetName
%% where
%%      Svr = pid()
%%      SetName = string()
%% @end
-spec(get_active_set(Svr::pid()) -> SetName::string()).
%%-----------------------------------------------------------------------------
get_active_set(Svr) ->
    gen_server:call(Svr, get_active_set).


%%-----------------------------------------------------------------------------
%% @doc Change the active set.
%% @spec set_active_set(Svr, SetName) -> ok | {error, Reason}
%% where
%%      Svr = pid()
%%      SetName = string()
%%      Reason = any()
%% @end
-spec(set_active_set(Svr::pid(), SetName::string()) ->
    ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
set_active_set(Svr, SetName) ->
    gen_server:call(Svr, {set_active_set, SetName}).


%%-----------------------------------------------------------------------------
%% @doc Remove a set.
%% @spec rem_set(Svr, SetName) -> ok | {error, Reason}
%% where
%%      Svr = pid()
%%      SetName = string()
%%      Reason = any()
%% @end
-spec(rem_set(Svr::pid(), SetName::string()) ->
    ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
rem_set(Svr, SetName) ->
    gen_server:call(Svr, {rem_set, SetName}).


%%-----------------------------------------------------------------------------
%% @doc Get a list of the key/value pairs in a set.
%% @spec get_values(Svr, SetName) -> Params | {error, Reason}
%% where
%%      Svr = pid()
%%      SetName = string()
%%      Params = [{string(), string()}]
%%      Reason = any()
%% @end
-spec(get_values(Svr::pid(), SetName::string()) ->
    [{string(), string()}] | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
get_values(Svr, SetName) ->
    gen_server:call(Svr, {get_values, SetName}).


%%-----------------------------------------------------------------------------
%% @doc Get a value from the currently active set.
%% @spec get_value(Svr, Key) -> Value
%% where
%%      Svr = pid()
%%      Key = string()
%%      Value = string() | {error, no_param}
%% @end
-spec(get_value(Svr::pid(), Key::string()) ->
    Value::string() | {error, no_param}).
%%-----------------------------------------------------------------------------
get_value(Svr, Key) ->
    gen_server:call(Svr, {get_value, Key}).


%%-----------------------------------------------------------------------------
%% @doc Get a value from a set.
%% @spec get_value(Svr, Set, Key) -> Value
%% where
%%      Svr = pid()
%%      Set = string()
%%      Key = string()
%%      Value = string() | {error, no_set | no_param}
%% @end
-spec(get_value(Svr::pid(), Set::string(), Key::string()) ->
    Value::string() | {error, no_set | no_param}).
%%-----------------------------------------------------------------------------
get_value(Svr, Set, Key) ->
    gen_server:call(Svr, {get_value, Set, Key}).


%%-----------------------------------------------------------------------------
%% @doc Set a value in the currently active set.
%% @spec set_value(Svr, Key, Value) -> ok | {error, Reason}
%% where
%%      Svr = pid()
%%      Key = string()
%%      Value = string()
%%      Reason = no_set | no_param
%% @end
-spec(set_value(Svr::pid(), Key::string(), Value::string()) ->
    ok | {error, Reason::no_set | no_param}).
%%-----------------------------------------------------------------------------
set_value(Svr, Key, Value) ->
    gen_server:call(Svr, {set_value, Key, Value}).


%%-----------------------------------------------------------------------------
%% @doc Set a value in a set.
%% @spec set_value(Svr, Set, Key, Value) -> ok | {error, Reason}
%% where
%%      Svr = pid()
%%      Set = string()
%%      Key = string()
%%      Value = string()
%%      Reason = no_set | no_param
%% @end
-spec(set_value(Svr::pid(), Set::string(), Key::string(), Value::string()) ->
    ok | {error, Reason::no_set | no_param}).
%%-----------------------------------------------------------------------------
set_value(Svr, Set, Key, Value) ->
    gen_server:call(Svr, {set_value, Set, Key, Value}).


%%-----------------------------------------------------------------------------
%% @doc Set all values in a set.
%% @spec set_values(Svr, Set, Values) -> ok | {error, Reason}
%% where
%%      Svr = pid()
%%      Set = string()
%%      Values = proplist()
%%      Reason = no_set | no_param
%% @end
-spec(set_values(Svr::pid(), Set::string(), Values::proplist()) ->
    ok | {error, Reason::no_set | no_param}).
%%-----------------------------------------------------------------------------
set_values(Svr, Set, Values) ->
    gen_server:call(Svr, {set_values, Set, Values}).


%%-----------------------------------------------------------------------------
%% @doc Check if a set exists.
%% @spec has_set(Svr, Set) -> boolean()
%% where
%%      Svr = pid()
%%      Set = string()
%% @end
-spec(has_set(Svr::pid(), Set::string()) -> boolean()).
%%-----------------------------------------------------------------------------
has_set(Svr, Set) ->
    gen_server:call(Svr, {has_set, Set}).


%%-----------------------------------------------------------------------------
%% @doc Check if a key exists in a set.
%% @spec has_key(Svr, Set, Key) -> boolean()
%% where
%%      Svr = pid()
%%      Set = string()
%%      Key = string()
%% @end
-spec(has_key(Svr::pid(), Set::string(), Key::string()) -> boolean()).
%%-----------------------------------------------------------------------------
has_key(Svr, Set, Key) ->
    gen_server:call(Svr, {has_key, Set, Key}).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the configuration server.
%% @spec init(Config) -> {ok, State} | {stop, Reason}
%% where
%%      FileName = string()
%%      Defaults = config()
%%      State = #cfg_sets{}
%%      Reason = any()
%% @end
-spec(init(Config::config()) ->
        {ok, #cfg_sets{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init(Config) ->
    ?LOG(rtl_info, "Initialising RTC configuration server ~p.", [self()]),
    ?LOG(rtl_debug, "Configuration is ~p", [Config]),
    State = config_set:init(Config),
    ?LOG(rtl_paranoid, "Initialisation complete; state is ~p", [State]),
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = get_sets | get_active_set | {set_active_set, SetName} |
%%          {rem_set, Set} | {get_value, Key} | {get_value, Set, Key} |
%%          {set_value, Key, Value} | {set_value, Set, Key, Value} |
%%          {set_values, Set, Values} | {has_set, SetName} |
%%          {has_key, SetName, Key}
%%      SetName = string()
%%      Set = string()
%%      Key = string()
%%      Value = string()
%%      Values = proplist()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #cfg_sets{}
%%      Reply = ok | {error, Reason} | Value
%%      Reason = no_set | no_param | active_set
%%      NewState = #cfg_sets{}
%% @end
-spec(handle_call(Request::get_sets | get_active_set |
            {set_active_set, SetName::string()} | {rem_set, Set::string()} |
            {get_value, Key::string()} |
            {get_value, Set::string(), Key::string()} |
            {set_value, Key::string(), Value::string()} |
            {set_value, Set::string(), Key::string(), Value::string()} |
            {set_values, Set::string(), Values::proplist()} |
            {has_set, Set::string()} | {has_key, Set::string, Key::string()},
        From::{pid(), Tag::any()}, State::#cfg_sets{}) ->
    {reply,
        Reply::ok | {error, Reason::no_param | no_set | active_set} | string(),
        NewState::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
handle_call(get_sets, _From, State) ->
    {reply, config_set:get_sets(State), State};
handle_call(get_active_set, _From, State) ->
    {reply, config_set:get_active_set(State), State};
handle_call({set_active_set, SetName}, _From, State) ->
    {Result, NewState} = config_set:set_active_set(SetName, State),
    {reply, Result, NewState};
handle_call({rem_set, SetName}, _From, State) ->
    {Result, NewState} = config_set:rem_set(SetName, State),
    {reply, Result, NewState};
handle_call({get_values, SetName}, _From, State) ->
    {reply, config_set:get_values(SetName, State), State};
handle_call({get_value, K}, _From, State) ->
    {reply, config_set:get_value(K, State), State};
handle_call({get_value, S, K}, _From, State) ->
    {reply, config_set:get_value(S, K, State), State};
handle_call({set_value, K, V}, _From, State) ->
    {Result, NewState} = config_set:set_value(K, V, State),
    {reply, Result, NewState};
handle_call({set_value, S, K, V}, _From, State) ->
    {Result, NewState} = config_set:set_value(S, K, V, State),
    {reply, Result, NewState};
handle_call({set_values, S, Vals}, _From, State) ->
    {reply, ok, config_set:set_values(S, Vals, State)};
handle_call({has_set, SetName}, _From, State) ->
    {reply, config_set:has_set(SetName, State), State};
handle_call({has_key, SetName, Key}, _From, State) ->
    {reply, config_set:has_key(SetName, Key, State), State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) -> {stop, normal, NewState}
%% where
%%      Request = stop
%%      State = #cfg_sets{}
%%      NewState = #cfg_sets{}
%% @end
-spec(handle_cast(Request::stop, State::#cfg_sets{}) ->
    {stop, normal, NewState::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {ok, NewState}
%% where
%%      Info = not_supported
%%      State = #cfg_sets{}
%%      NewState = #cfg_sets{}
%% @end
-spec(handle_info(Info::not_supported, State::#cfg_sets{}) ->
    {ok, NewState::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = #cfg_sets{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#cfg_sets{}) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, _State) ->
    ?LOG(rtl_info, "~p shutting down normally.", [self()]);
terminate(shutdown, _State) ->
    ?LOG(rtl_info, "~p shut down by supervisor.", [self()]);
terminate({shutdown, Reason}, _State) ->
    ?LOG(rtl_info, "~p shutting down: ~p", [self(), Reason]);
terminate(Reason, _State) ->
    ?LOG(rtl_info, "~p unusual shutdown: ~p", [self(), Reason]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = #cfg_sets{}
%%      Extra = any()
%%      NewState = #cfg_sets{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#cfg_sets{},
        Extra::any()) -> {ok, NewState::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

