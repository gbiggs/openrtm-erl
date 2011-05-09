%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Utilities for managing SDO name/value lists.
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
-module(nvlist).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("orber/include/corba.hrl").
-include("idl/SDOPackage.hrl").
-include("type_specs.hrl").
-include("log.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([to_dict/1, from_dict/1, to_list/1, from_list/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Converts an NVList into a dictionary.
%% @spec to_dict(NVList) -> dict()
%% where
%%      NVList = [#'SDOPackage_NameValue'{}]
%% @end
-spec(to_dict(NVLIst::[#'SDOPackage_NameValue'{}]) -> dict()).
%%-----------------------------------------------------------------------------
to_dict(NVList) ->
    dict:from_list(to_list(NVList)).


%%-----------------------------------------------------------------------------
%% @doc Converts a dictionary into an NVList.
%% @spec from_dict(dict()) -> NVList
%% where
%%      NVList = [#'SDOPackage_NameValue'{}]
%% @end
-spec(from_dict(dict()) -> [#'SDOPackage_NameValue'{}]).
%%-----------------------------------------------------------------------------
from_dict(Dict) ->
    from_list(dict:to_list(Dict)).


%%-----------------------------------------------------------------------------
%% @doc Converts an NVList into a list of tuples.
%% @spec to_list(NVList) -> [{Name, Value}]
%% where
%%      NVList = [#'SDOPackage_NameValue'{}]
%%      Name = string()
%%      Value = any()
%% @end
-spec(to_list(NVLIst::[#'SDOPackage_NameValue'{}]) ->
    [{Name::string(), Value::any()}]).
%%-----------------------------------------------------------------------------
to_list(NVList) ->
    lists:map(fun nv_to_tuple/1, NVList).


%%-----------------------------------------------------------------------------
%% @doc Converts a dictionary into an NVList.
%% @spec from_list([{Name, Value}]) -> NVList
%% where
%%      Name = string()
%%      Value = any()
%%      NVList = [#'SDOPackage_NameValue'{}]
%% @end
-spec(from_list([{Name::string(), Value::any()}]) ->
    NVLIst::[#'SDOPackage_NameValue'{}]).
%%-----------------------------------------------------------------------------
from_list(List) ->
    lists:map(fun tuple_to_nv/1, List).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Convert an SDO name/value pair into a tuple.
%% @spec nv_to_tuple(NV) -> {Name, Value}
%% where
%%      NV = #'SDOPackage_NameValue'{}
%%      Name = string()
%%      Value = any()
%% @end
-spec(nv_to_tuple(NV::#'SDOPackage_NameValue'{}) -> {string(), any()}).
%%-----------------------------------------------------------------------------
nv_to_tuple(#'SDOPackage_NameValue'{name=Name, value=Any}) ->
    {Name, any:get_value(Any)}.


%%-----------------------------------------------------------------------------
%% @doc Convert a tuple into a SDO name/value pair.
%% @spec tuple_to_nv({Name, Value}) -> NV
%% where
%%      Name = string()
%%      Value = any()
%%      NV = #'SDOPackage_NameValue'{}
%% @end
-spec(tuple_to_nv({string(), any()}) -> NV::#'SDOPackage_NameValue'{}).
%%-----------------------------------------------------------------------------
tuple_to_nv({Name, Value}) when is_list(Value) ->
    TC = orber_tc:string(0),
    Any = any:create(TC, Value),
    #'SDOPackage_NameValue'{name=Name, value=Any}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test converting to and from a dict.
%% @spec dict_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(dict_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
dict_test_() ->
    [?LET(D, dict:from_list([{"name1", "1"}, {"name2", "2"}, {"name3", "3"}]),
            ?_assertEqual(D, to_dict(from_dict(D))))].


%%-----------------------------------------------------------------------------
%% @doc Test converting to a list.
%% @spec to_list_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(to_list_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
to_list_test_() ->
    {setup,
        fun() ->
            case whereis(orber_sup)
                of undefined ->
                    orber:jump_start([{iiop_port,2809}])
                 ; _ ->
                    ok
            end
        end,
        fun(_) -> orber:uninstall() end,
        fun(_) ->
            [?_assertMatch([{"name1", "1"}],
                    to_list([#'SDOPackage_NameValue'{name="name1",
                                value=#any{typecode={tk_string,0},
                                    value="1"}}])),
                ?_assertMatch([{"name1", "1"}, {"name2", "2"}],
                    to_list([#'SDOPackage_NameValue'{name="name1",
                                value=#any{typecode={tk_string,0},
                                    value="1"}},
                            #'SDOPackage_NameValue'{name="name2",
                                value=#any{typecode={tk_string,0},
                                    value="2"}}]))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test converting from a list.
%% @spec from_list_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(from_list_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
from_list_test_() ->
    {setup,
        fun() ->
            case whereis(orber_sup)
                of undefined ->
                    orber:jump_start([{iiop_port,2809}])
                 ; _ ->
                    ok
            end
        end,
        fun(_) -> orber:uninstall() end,
        fun(_) ->
            [?_assertMatch([#'SDOPackage_NameValue'{name="name1",
                            value=#any{value="1"}}],
                    from_list([{"name1", "1"}])),
                ?_assertMatch([#'SDOPackage_NameValue'{name="name1",
                            value=#any{value="1"}},
                        #'SDOPackage_NameValue'{name="name2",
                            value=#any{value="2"}}],
                    from_list([{"name1", "1"}, {"name2", "2"}]))]
        end
    }.

-endif. % TEST

