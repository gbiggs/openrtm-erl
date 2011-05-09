%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Configuration sets.
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
-module(config_set).


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
-export([init/0, init/1]).
-export([get_sets/1, get_active_set/1, set_active_set/2, rem_set/2]).
-export([get_values/2, set_values/3]).
-export([get_value/2, get_value/3, set_value/3, set_value/4]).
-export([has_set/2, has_key/3]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create an empty configuration set.
%% @spec init() -> #cfg_sets{}
%% @end
-spec(init() -> #cfg_sets{}).
%%-----------------------------------------------------------------------------
init() ->
    #cfg_sets{}.


%%-----------------------------------------------------------------------------
%% @doc Create a configuration set based on a configuration.
%% @spec init(Config) -> #cfg_sets{}
%% where
%%      Config = config()
%% @end
-spec(init(Config::config()) -> #cfg_sets{}).
%%-----------------------------------------------------------------------------
init(Config) ->
    Sets = get_sets_from_config(Config),
    {ActName, ActSet} = get_active_from_config(Config, Sets),
    #cfg_sets{sets=Sets, act=ActSet, act_name=ActName}.


%%-----------------------------------------------------------------------------
%% @doc Get the available configuration sets.
%% @spec get_sets(CSets) -> SetNames
%% where
%%      CSets = #cfg_sets{}
%%      SetNames = [string()]
%% @end
-spec(get_sets(CSets::#cfg_sets{}) -> SetNames::[string()]).
%%-----------------------------------------------------------------------------
get_sets(#cfg_sets{sets=Sets}) ->
    proplists:get_keys(Sets).


%%-----------------------------------------------------------------------------
%% @doc Get the active set.
%% @spec get_active_set(CSets) -> SetName
%% where
%%      CSets = #cfg_sets{}
%%      SetName = string()
%% @end
-spec(get_active_set(CSets::#cfg_sets{}) -> SetName::string()).
%%-----------------------------------------------------------------------------
get_active_set(#cfg_sets{act_name=N}) ->
    N.


%%-----------------------------------------------------------------------------
%% @doc Change the active set.
%% @spec set_active_set(SetName, CSets) -> Result
%% where
%%      SetName = string()
%%      CSets = #cfg_sets{}
%%      Result = {ok | {error, no_set}, NewCSets}
%%      NewCSets = #cfg_sets{}
%% @end
-spec(set_active_set(SetName::string(), CSets::#cfg_sets{}) ->
    {ok | {error, no_set}, NewCSets::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
set_active_set(SetName, #cfg_sets{sets=Sets}=CSets) ->
    case proplists:get_value(SetName, Sets)
        of undefined ->
            {{error, no_set}, CSets}
         ; Set ->
            {ok, CSets#cfg_sets{act_name=SetName, act=Set}}
    end.


%%-----------------------------------------------------------------------------
%% @doc Remove a set.
%% @spec rem_set(SetName, CSets) -> Result
%% where
%%      SetName = string()
%%      CSets = #cfg_sets{}
%%      Result = {ok | {error, no_set | active_set}, NewCSets}
%%      NewCSets = #cfg_sets{}
%% @end
-spec(rem_set(SetName::string(), CSets::#cfg_sets{}) ->
    {ok | {error, no_set | active_set}, NewCSets::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
rem_set(SetName, #cfg_sets{act_name=SetName}=CSets) ->
    {{error, active_set}, CSets};
rem_set(SetName, #cfg_sets{sets=Sets}=CSets) ->
    case proplists:get_value(SetName, Sets)
        of undefined ->
            {{error, no_set}, CSets}
         ; _ ->
            {ok, CSets#cfg_sets{sets=proplists:delete(SetName, Sets)}}
    end.


%%-----------------------------------------------------------------------------
%% @doc Get a list of the key/value pairs in the set.
%% @spec get_param(SetName, CSets) -> Result
%% where
%%      SetName = string()
%%      CSets = #cfg_sets{}
%%      Result = [{string(), string()}] | {error, no_set}
%% @end
-spec(get_values(SetName::string(), CSets::#cfg_sets{}) ->
    Value::[{string(), string()}] | {error, no_param}).
%%-----------------------------------------------------------------------------
get_values(SetName, CSets) ->
    case proplists:get_value(SetName, CSets#cfg_sets.sets)
        of undefined ->
            {error, no_set}
         ; Set ->
            Set
    end.


%%-----------------------------------------------------------------------------
%% @doc Set all keys in a set at once, creating a new set if necessary.
%% @spec set_values(SetName, Values, CSets) -> Result
%% where
%%      SetName = string()
%%      Values = proplist()
%%      CSets = #cfg_sets{}
%%      Result = [{string(), string()}]
%% @end
-spec(set_values(SetName::string(), Values::proplist(), CSets::#cfg_sets{}) ->
    Value::[{string(), string()}]).
%%-----------------------------------------------------------------------------
set_values(SetName, Values, #cfg_sets{act_name=SetName}=CSets) ->
    CSets#cfg_sets{sets=[{SetName, Values} | proplists:delete(SetName,
                CSets#cfg_sets.sets)], act=Values};
set_values(SetName, Values, CSets) ->
    CSets#cfg_sets{sets=[{SetName, Values} | proplists:delete(SetName,
                CSets#cfg_sets.sets)]}.


%%-----------------------------------------------------------------------------
%% @doc Get a value from the currently active set.
%% @spec get_value(Key, CSets) -> Result
%% where
%%      Key = string()
%%      CSets = #cfg_sets{}
%%      Result = string() | {error, no_param}
%% @end
-spec(get_value(Key::string(), CSets::#cfg_sets{}) ->
    Value::string() | {error, no_param}).
%%-----------------------------------------------------------------------------
get_value(Key, #cfg_sets{act=Set}) ->
    get_val_from_set(Key, Set).


%%-----------------------------------------------------------------------------
%% @doc Get a value from a set.
%% @spec get_value(Set, Key, CSets) -> Result
%% where
%%      Set = string()
%%      Key = string()
%%      CSets = #cfg_sets{}
%%      Result = string() | {error, no_set | no_param}
%% @end
-spec(get_value(Set::string(), Key::string(), CSets::#cfg_sets{}) ->
    Value::string() | {error, no_set | no_param}).
%%-----------------------------------------------------------------------------
get_value(Set, Key, #cfg_sets{sets=Sets}) ->
    get_val_from_sets(Set, Key, Sets).


%%-----------------------------------------------------------------------------
%% @doc Set a value in the currently active set.
%% @spec set_value(Key, Value, CSets) -> Result
%% where
%%      CSets = #cfg_sets{}
%%      Key = string()
%%      Value = string()
%%      Reason = {ok | {error, no_set | no_param}, NewCSets}
%%      NewCSets = #cfg_sets{}
%% @end
-spec(set_value(Key::string(), Value::string(), CSets::#cfg_sets{}) ->
    {ok | {error, no_set | no_param}, NewCSets::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
set_value(Key, Value, CSets) ->
    set_value_in_active_set(Key, Value, CSets).


%%-----------------------------------------------------------------------------
%% @doc Set a value in a set.
%% @spec set_value(Set, Key, Value, CSets) -> Result
%% where
%%      Set = string()
%%      Key = string()
%%      Value = string()
%%      CSets = #cfg_sets{}
%%      Result = {ok | {error, no_set | no_param}, NewCSets}
%%      NewCSets = #cfg_sets{}
%% @end
-spec(set_value(Set::string(), Key::string(),
        Value::string(), CSets::#cfg_sets{}) ->
    {ok | {error, no_set | no_param}, NewCSets::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
set_value(Set, Key, Value, #cfg_sets{act_name=Set}=CSets) ->
    set_value_in_active_set(Key, Value, CSets);
set_value(Set, Key, Value, CSets) ->
    set_value_in_sets(Set, Key, Value, CSets).


%%-----------------------------------------------------------------------------
%% @doc Check if a set exists.
%% @spec has_set(Set, CSets) -> boolean()
%% where
%%      Set = string()
%%      CSets = #cfg_sets{}
%% @end
-spec(has_set(Set::string(), CSets::#cfg_sets{}) -> boolean()).
%%-----------------------------------------------------------------------------
has_set(Set, #cfg_sets{sets=Sets}) ->
    proplists:is_defined(Set, Sets).


%%-----------------------------------------------------------------------------
%% @doc Check if a key exists in a set.
%% @spec has_key(SetName, Key, CSets) -> boolean()
%% where
%%      SetName = string()
%%      Key = string()
%%      CSets = #cfg_sets{}
%% @end
-spec(has_key(SetName::string(), Key::string(), CSets::#cfg_sets{}) ->
    boolean()).
%%-----------------------------------------------------------------------------
has_key(SetName, Key, #cfg_sets{sets=Sets}) ->
    case proplists:get_value(SetName, Sets)
        of undefined ->
            false
         ; Set ->
            proplists:is_defined(Key, Set)
    end.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Get the configuration sets from a config.
%% @spec get_sets_from_config(Config) -> Sets
%% where
%%      Config = config()
%%      Sets = config_sets()
%% @end
-spec(get_sets_from_config(Config::config()) -> Sets::config_sets()).
%%-----------------------------------------------------------------------------
get_sets_from_config(Config) ->
    case config:get_value("conf", Config)
        of undefined ->
            [{"default", []}]
         ; Sets ->
            % foldr is used to preserve the original order
            lists:foldr(fun({_, V}=S, Acc) ->
                    case V
                        of [{_, _}|_] ->
                            [S | Acc]
                         ; _ ->
                            ?LOG(rtl_error, "Bad configuration set: ~p", [S]),
                            Acc
                    end
                end, [], Sets)
    end.


%%-----------------------------------------------------------------------------
%% @doc Get the default active set from a config. If the specified set is not
%% found, the first config set will be used. If no set is specified, "default"
%% will be used.
%% @spec get_active_from_config(Config, Sets) -> {SetName, Set}
%% where
%%      Config = config()
%%      Sets = config_sets()
%%      SetName = string()
%%      Set = config_set()
%% @end
-spec(get_active_from_config(Config::config(), Sets::config_sets()) ->
    {SetName::string(), Set::config_set()}).
%%-----------------------------------------------------------------------------
get_active_from_config(Config, Sets) ->
    case config:get_value(["configuration", "active_config"], Config)
        of undefined ->
            get_active_from_config1("default", Sets)
         ; Act ->
            get_active_from_config1(Act, Sets)
    end.

get_active_from_config1(_, []) ->
    {"default", []};
get_active_from_config1(Act, [{N, V}|_]=Sets) ->
    case proplists:get_value(Act, Sets)
        of undefined ->
            ?LOG(rtl_warn, "Specified active set ~p does not exist.", [Act]),
            {N, V}
         ; Set ->
            {Act, Set}
    end.


%%-----------------------------------------------------------------------------
%% @doc Get a value from a specific set.
%% @spec get_val_from_sets{SetName, Key, Sets} -> Result
%% where
%%      SetName = string()
%%      Key = string()
%%      Sets = config_sets()
%%      Result = Value | {error, no_set | no_param}
%% @end
-spec(get_val_from_sets(SetName::string(), Key::string(),
        Sets::config_sets()) ->
    Value::string() | undefined | {error, no_set}).
%%-----------------------------------------------------------------------------
get_val_from_sets(SetName, Key, Sets) ->
    case proplists:get_value(SetName, Sets)
        of undefined ->
            {error, no_set}
         ; Set ->
            get_val_from_set(Key, Set)
    end.


%%-----------------------------------------------------------------------------
%% @doc Get a value from a set.
%% @spec get_val_from_set{Key, Set} -> Result
%% where
%%      Key = string()
%%      Set = config_set()
%%      Result = Value | {error, no_param}
%% @end
-spec(get_val_from_set(Key::string(), Set::config_set()) ->
    Value::string() | {error, no_param}).
%%-----------------------------------------------------------------------------
get_val_from_set(Key, Set) ->
    case proplists:get_value(Key, Set)
        of undefined ->
            {error, no_param}
         ; Value ->
            Value
    end.


%%-----------------------------------------------------------------------------
%% @doc Set a value in the active set.
%% @spec set_value_in_active_set(Key, Value, CSets) -> Result
%% where
%%      Key = string()
%%      Value = string()
%%      CSets = #cfg_sets{}
%%      Result = {ok | {error, no_param}, NewCSets}
%%      NewCSets = #cfg_sets{}
%% @end
-spec(set_value_in_active_set(Key::string(), Value::string(),
        CSets::#cfg_sets{}) ->
    {ok | {error, no_param}, NewCSets::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
set_value_in_active_set(Key, Value, #cfg_sets{act_name=SetName}=CSets) ->
    {Result, NewCSets} = set_value_in_sets(SetName, Key, Value, CSets),
    NewActSet = proplists:get_value(SetName, NewCSets#cfg_sets.sets),
    {Result, NewCSets#cfg_sets{act=NewActSet}}.


%%-----------------------------------------------------------------------------
%% @doc Set a value in a specified set.
%% @spec set_value_in_sets(SetName, Key, Value, CSets) -> Result
%% where
%%      SetName = string()
%%      Key = string()
%%      Value = string()
%%      CSets = #cfg_sets{}
%%      Result = {ok | {error, no_set | no_param}, NewCSets}
%%      NewCSets = #cfg_sets{}
%% @end
-spec(set_value_in_sets(SetName::string(), Key::string(), Value::string(),
        CSets::#cfg_sets{}) ->
    {ok | {error, no_set | no_param}, NewCSets::#cfg_sets{}}).
%%-----------------------------------------------------------------------------
set_value_in_sets(SetName, Key, Value, #cfg_sets{sets=Sets}=CSets) ->
    case proplists:get_value(SetName, Sets)
        of undefined ->
            {{error, no_set}, CSets}
         ; Set ->
            {Result, NewSet} = set_value_in_set(Key, Value, Set),
            NewSets = proplists:delete(SetName, Sets),
            {Result, CSets#cfg_sets{sets=[{SetName, NewSet} | NewSets]}}
    end.


%%-----------------------------------------------------------------------------
%% @doc Set a value in a set.
%% @spec set_value_in_set(Key, Value, Set) -> Result
%% where
%%      Key = string()
%%      Value = string()
%%      Set = config_set()
%%      Result = {ok, NewSet} | {{error, no_param}, Set}
%%      NewSet = config_set()
%% @end
-spec(set_value_in_set(Key::string(), Value::string(), Set::config_set()) ->
    {ok | {error, no_param}, Set::config_set()}).
%%-----------------------------------------------------------------------------
set_value_in_set(Key, Value, Set) ->
    case proplists:is_defined(Key, Set)
        of true ->
            NewSet = proplists:delete(Key, Set),
            {ok, [{Key, Value} | NewSet]}
         ; false ->
            {ok, [{Key, Value} | Set]}
    end.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

