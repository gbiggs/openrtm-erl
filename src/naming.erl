%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Functions for managing names.
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
-module(naming).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming_NamingContext.hrl").
-include_lib("orber/src/orber_iiop.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([format_name/1, format_name/2, str_name_to_corba/1,
        full_address/1, register_below_nc/3, deregister_below_nc/2,
        start_orber/0]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Equivalent to format_name(Name, []).
%% @spec format_name(Name) -> string()
%% where
%%      Name = string()
%% @end
-spec(format_name(Name::string()) -> string()).
%%-----------------------------------------------------------------------------
format_name(Name) ->
    format_name(Name, []).


%%-----------------------------------------------------------------------------
%% @doc Format a string name in the manner of printf using a combination of the
%% provided configuration and values from the configuration server.
%% @spec format_name(Name, C) -> string()
%% where
%%      Name = string()
%%      C = config()
%% @end
-spec(format_name(Name::string(), C::config()) -> string()).
%%-----------------------------------------------------------------------------
format_name(Name, C) ->
    format_name1(Name, C, []).

format_name1([], _, Acc) ->
    lists:reverse(lists:flatten(Acc));
format_name1([$%,$n|T], C, Acc) ->
    Val = lists:reverse(config:val_to_str(
            config:get_value(["instance_name"], C))),
    format_name1(T, C, [Val|Acc]);
format_name1([$%,$t|T], C, Acc) ->
    Val = lists:reverse(config:val_to_str(
            config:get_value(["type_name"], C))),
    format_name1(T, C, [Val|Acc]);
format_name1([$%,$m|T], C, Acc) ->
    Val = lists:reverse(config:val_to_str(
            config:get_value(["type_name"], C))),
    format_name1(T, C, [Val|Acc]);
format_name1([$%,$v|T], C, Acc) ->
    Val = lists:reverse(config:val_to_str(
            config:get_value(["version"], C))),
    format_name1(T, C, [Val|Acc]);
format_name1([$%,$V|T], C, Acc) ->
    Val = lists:reverse(config:val_to_str(
            config:get_value(["vendor"], C))),
    format_name1(T, C, [Val|Acc]);
format_name1([$%,$c|T], C, Acc) ->
    Val = lists:reverse(config:val_to_str(
            config:get_value(["category"], C))),
    format_name1(T, C, [Val|Acc]);
format_name1([$%,$h|T], C, Acc) ->
    Val = lists:reverse(atom_to_list(node())),
    format_name1(T, C, [Val|Acc]);
format_name1([$%,$M|T], C, Acc) ->
    Val = lists:reverse(config:val_to_str(
            config_svr:get_value(["manager", "name"]))),
    format_name1(T, C, [Val|Acc]);
format_name1([H|T], C, Acc) ->
    format_name1(T, C, [H|Acc]).


%%-----------------------------------------------------------------------------
%% @doc Convert a name in string format to a CORBA name (using lname:new/1).
%% The result is suitable for use with naming services.
%% @spec str_name_to_corba(Name) -> [#'CosNaming_NameComponent'{}]
%% where
%%      Name = string()
%% @end
-spec(str_name_to_corba(Name::string()) -> [#'CosNaming_NameComponent'{}]).
%%-----------------------------------------------------------------------------
str_name_to_corba(Name) ->
    str_name_to_corba1(string:tokens(Name, "/"), []).

str_name_to_corba1([], Acc) ->
    lists:reverse(Acc);
str_name_to_corba1([H|T], Acc) ->
    str_name_to_corba1(T,
        [make_name_comp(list_to_tuple(string:tokens(H, ".")))|Acc]).


%%-----------------------------------------------------------------------------
%% @doc Get the full address of a name server.
%% @spec full_address(A) -> FullAddress
%% where
%%      A = string()
%%      FullAddress = string()
%% @end
-spec(full_address(A::string()) -> FullAddress::string()).
%%-----------------------------------------------------------------------------
full_address(A) ->
    lists:append(["corbaloc:iiop:", A, "/NameService"]).


%%-----------------------------------------------------------------------------
%% @doc Register a name in a naming context, creating sub-contexts if
%% necessary. Returns the naming context to which the object was ultimately
%% registered. If Name only has one member, this will be the same as NC.
%% @spec register_below_nc(NC, Name, Obj) -> NC2 | {error, Reason}
%% where
%%      NC = object_ref()
%%      Name = [#'CosNaming_NameComponent'{}]
%%      Obj = object_ref()
%%      NC2 = object_ref()
%%      Reason = any()
%% @end
-spec(register_below_nc(NC::object_ref(), Name::[#'CosNaming_NameComponent'{}],
        Obj::object_ref()) -> object_ref() | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
register_below_nc(NC, Name, Obj) ->
    register_below_nc1(NC, Name, Obj).

register_below_nc1(NC, [H], Obj) ->
    try 'CosNaming_NamingContext':bind(NC, [H], Obj)
        of ok ->
            NC
    catch
        throw:{'EXCEPTION', #'CosNaming_NamingContext_AlreadyBound'{}} ->
            {error, already_bound}
    end;
register_below_nc1(NC, [H|T], Obj) ->
    NextNC = try 'CosNaming_NamingContext':resolve(NC, [H])
        of N ->
            N
    catch
        throw:{'EXCEPTION', #'CosNaming_NamingContext_NotFound'{}} ->
            'CosNaming_NamingContext':bind_new_context(NC, [H])
    end,
    register_below_nc1(NextNC, T, Obj).


%%-----------------------------------------------------------------------------
%% @doc Remove a registration from a naming context.
%% @spec deregister_below_nc(NC, Name) -> ok | {error, Reason}
%% where
%%      NC = object_ref()
%%      Name = [#'CosNaming_NameComponent'{}] | string()
%%      Reason = any()
%% @end
-spec(deregister_below_nc(NC::object_ref(),
        Name::[#'CosNaming_NameComponent'{}] | string()) ->
    ok | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
deregister_below_nc(NC, [#'CosNaming_NameComponent'{}|_]=Name) ->
    ?LOG(rtl_paranoid, "De-registering ~p from context ~p", [Name, NC]),
    'CosNaming_NamingContext':unbind(NC, Name);
deregister_below_nc(NC, Name) ->
    deregister_below_nc(NC, str_name_to_corba(Name)).


%%-----------------------------------------------------------------------------
%% @doc Start the Orber application.
%% @spec start_orber() -> ok
%% @end
-spec(start_orber() -> ok).
%%-----------------------------------------------------------------------------
start_orber() ->
    case whereis(orber_sup)
        of undefined ->
            ?LOG(rtl_info, "Starting orber"),
            mnesia:start(),
            corba:orb_init([{domain, "OpenRTM-erlang"},
                    {iiop_connection_timeout, 120},
                    %{orber_debug_level, 10},
                    %{interceptors, {native, [orber_iiop_tracer]}},
                    {flags,16#80}]), % 0x80 = Light IFR
            orber:install([node()], [{ifr_storage_type, ram_copies}]),
            orber:start(),
            oe_SDOPackage:oe_register(),
            oe_RTC:oe_register(),
            oe_OpenRTM:oe_register(),
            oe_DataPort:oe_register(),
            oe_BasicDataType:oe_register(),
            oe_Manager:oe_register()
         ; _ ->
            ok
    end.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Turn a list of name parts into a CORBA name component.
%% @spec make_name_comp({ID, Kind} | {ID}) -> #'CosNaming_NameComponent'{}
%% where
%%      ID = string()
%%      Kind = string()
%% @end
-spec(make_name_comp({ID::string(), Kind::string()} | {ID::string()}) ->
        #'CosNaming_NameComponent'{}).
%%-----------------------------------------------------------------------------
make_name_comp({ID, Kind}) ->
    NC = lname_component:create(),
    NC1 = lname_component:set_id(NC, ID),
    lname_component:set_kind(NC1, Kind);
make_name_comp({ID}) ->
    NC = lname_component:create(),
    lname_component:set_id(NC, ID).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test formatting names.
%% @spec format_name_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(format_name_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
format_name_test_() ->
    {setup,
        fun() ->
            start_mock_config_svr(defaults:defaults()),
            make_comp_cfg()
        end,
        fun(_) -> stop_mock_config_svr() end,
        fun(C) ->
            [?_assertMatch("name.mgr", format_name("name.mgr")),
                ?_assertMatch("name.mgr", format_name("name.mgr", C)),
                ?_assertMatch("blurgle/name.rtc",
                    format_name("blurgle/name.rtc", C)),
                ?_assertMatch("name.rtc", format_name("name.rtc", C)),
                ?_assertMatch("comp_inst.rtc", format_name("%n.rtc", C)),
                ?_assertMatch("comp_type.rtc", format_name("%t.rtc", C)),
                ?_assertMatch("comp_type.rtc", format_name("%m.rtc", C)),
                ?_assertMatch("comp_vers.rtc", format_name("%v.rtc", C)),
                ?_assertMatch("comp_vend.rtc", format_name("%V.rtc", C)),
                ?_assertMatch("comp_cat.rtc", format_name("%c.rtc", C)),
                ?_assertEqual(atom_to_list(node()) ++ ".rtc",
                    format_name("%h.rtc", [])),
                ?_assertMatch("manager.rtc", format_name("%M.rtc", [])),
                ?_assertMatch("manager.rtc", format_name("%M.rtc")),
                ?_assertEqual(atom_to_list(node()) ++
                    ".host/name_comp_inst.rtc",
                    format_name("%h.host/name_%n.rtc", C)),
                ?_assertEqual(atom_to_list(node()) ++ ".host/name_.rtc",
                    format_name("%h.host/name_%n.rtc", []))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test converting string names to CORBA structures.
%% @spec str_name_to_corba_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(str_name_to_corba_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
str_name_to_corba_test_() ->
    [?_assertMatch([#'CosNaming_NameComponent'{id="name", kind="rtc"}],
            str_name_to_corba("name.rtc")),
        ?_assertMatch([#'CosNaming_NameComponent'{id="hostname", kind="host"},
                #'CosNaming_NameComponent'{id="name", kind="rtc"}],
            str_name_to_corba("hostname.host/name.rtc")),
        ?_assertMatch([#'CosNaming_NameComponent'{id="hostname", kind=""},
                #'CosNaming_NameComponent'{id="name", kind="rtc"}],
            str_name_to_corba("hostname/name.rtc")),
        ?_assertMatch([#'CosNaming_NameComponent'{id="hostname", kind="host"},
                #'CosNaming_NameComponent'{id="name", kind=""}],
            str_name_to_corba("hostname.host/name"))].


%%-----------------------------------------------------------------------------
%% @doc Test converting server address to full CORBA addresses.
%% @spec full_address_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(full_address_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
full_address_test_() ->
    [?_assertMatch("corbaloc:iiop:localhost/NameService",
            full_address("localhost"))].


%%-----------------------------------------------------------------------------
%% @doc Test registering an object under a naming context.
%% @spec register_below_nc_1_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(register_below_nc_1_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
register_below_nc_1_test_() ->
    {setup,
        fun() ->
            case whereis(orber_sup)
                of undefined ->
                    orber:jump_start([{iiop_port,28090}])
                 ; _ ->
                    ok
            end,
            % Create a naming service
            corba:resolve_initial_references("NameService")
        end,
        fun(_) -> orber:uninstall() end,
        fun(NS) ->
            [?_assertEqual(NS, naming:register_below_nc(NS,
                        naming:str_name_to_corba("my_context"), none)),
                ?_assert(object_is_regged(NS,
                        naming:str_name_to_corba("my_context")))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test registering an object under a naming context.
%% @spec register_below_nc_2_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(register_below_nc_2_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
register_below_nc_2_test_() ->
    {setup,
        fun() ->
            case whereis(orber_sup)
                of undefined ->
                    orber:jump_start([{iiop_port,28090}])
                 ; _ ->
                    ok
            end,
            % Create a naming service
            NS = corba:resolve_initial_references("NameService"),
            Name = naming:str_name_to_corba("blurgle/splort.stuff"),
            naming:register_below_nc(NS, Name, none),
            {NS, Name}
        end,
        fun(_) -> orber:uninstall() end,
        fun({NS, Name}) ->
            [?_assert(object_is_regged(NS, Name))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test registering an object under a naming context.
%% @spec register_below_nc_3_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(register_below_nc_3_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
register_below_nc_3_test_() ->
    {setup,
        fun() ->
            case whereis(orber_sup)
                of undefined ->
                    orber:jump_start([{iiop_port,28090}])
                 ; _ ->
                    ok
            end,
            % Create a naming service
            NS = corba:resolve_initial_references("NameService"),
            Name = naming:str_name_to_corba("blurgle/blorg"),
            naming:register_below_nc(NS, Name, none),
            {NS, Name}
        end,
        fun(_) -> orber:uninstall() end,
        fun({NS, Name}) ->
            [?_assert(object_is_regged(NS, Name))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test registering an object under a naming context.
%% @spec register_below_nc_4_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(register_below_nc_4_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
register_below_nc_4_test_() ->
    {setup,
        fun() ->
            case whereis(orber_sup)
                of undefined ->
                    orber:jump_start([{iiop_port,28090}])
                 ; _ ->
                    ok
            end,
            % Create a naming service
            NS = corba:resolve_initial_references("NameService"),
            Name = naming:str_name_to_corba("blarg/blorg"),
            naming:register_below_nc(NS, Name, none),
            {NS, Name}
        end,
        fun(_) -> orber:uninstall() end,
        fun({NS, Name}) ->
            [?_assert(object_is_regged(NS, Name))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test deregistering an object from a naming context.
%% @spec deregister_below_nc_1_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(deregister_below_nc_1_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
deregister_below_nc_1_test_() ->
    {setup,
        fun() ->
            case whereis(orber_sup)
                of undefined ->
                    orber:jump_start([{iiop_port,28090}])
                 ; _ ->
                    ok
            end,
            % Create a naming service and put something in it
            NS = corba:resolve_initial_references("NameService"),
            Name = naming:str_name_to_corba("blarg/blorg"),
            naming:register_below_nc(NS, Name, none),
            {NS, Name}
        end,
        fun(_) -> orber:uninstall() end,
        fun({NS, Name}) ->
            [?_assertMatch(ok, deregister_below_nc(NS, Name)),
                ?_assertNot(object_is_regged(NS, Name))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test deregistering an object from a naming context.
%% @spec deregister_below_nc_2_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(deregister_below_nc_2_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
deregister_below_nc_2_test_() ->
    {setup,
        fun() ->
            case whereis(orber_sup)
                of undefined ->
                    orber:jump_start([{iiop_port,28090}])
                 ; _ ->
                    ok
            end,
            % Create a naming service and put something in it
            NS = corba:resolve_initial_references("NameService"),
            Name = naming:str_name_to_corba("blorg"),
            naming:register_below_nc(NS, Name, none),
            {NS, Name}
        end,
        fun(_) -> orber:uninstall() end,
        fun({NS, Name}) ->
            [?_assertMatch(ok, deregister_below_nc(NS, "blorg")),
                ?_assertNot(object_is_regged(NS, Name))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test deregistering an object from a naming context.
%% @spec deregister_below_nc_3_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(deregister_below_nc_3_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
deregister_below_nc_3_test_() ->
    {setup,
        fun() ->
            case whereis(orber_sup)
                of undefined ->
                    orber:jump_start([{iiop_port,28090}])
                 ; _ ->
                    ok
            end,
            % Create a naming service and put something in it
            NS = corba:resolve_initial_references("NameService"),
            Name = naming:str_name_to_corba("blurgle"),
            naming:register_below_nc(NS, Name, none),
            {NS, Name}
        end,
        fun(_) -> orber:uninstall() end,
        fun({NS, Name}) ->
            [?_assertMatch(ok, deregister_below_nc(NS, "splurt")),
                ?_assert(object_is_regged(NS, Name))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Start a configuration server to provide data during the unit tests.
%% @spec start_mock_config_svr(Defaults) -> {ok, pid()}
%% where
%%      Defaults = config()
%% @end
-spec(start_mock_config_svr(Defaults::config()) -> {ok, pid()}).
%%-----------------------------------------------------------------------------
start_mock_config_svr(Defaults) ->
    {ok, _} = config_svr:start_link("", Defaults).


%%-----------------------------------------------------------------------------
%% @doc Stop the configuration server.
%% @spec stop_mock_config_svr() -> ok
%% @end
-spec(stop_mock_config_svr() -> ok).
%%-----------------------------------------------------------------------------
stop_mock_config_svr() ->
    config_svr:stop().


%%-----------------------------------------------------------------------------
%% @doc Make a fake component configuration.
%% @spec make_comp_cfg() -> config()
%% @end
-spec(make_comp_cfg() -> config()).
%%-----------------------------------------------------------------------------
make_comp_cfg() ->
    [{"instance_name", "comp_inst"},
        {"type_name", "comp_type"},
        {"version", "comp_vers"},
        {"vendor", "comp_vend"},
        {"category", "comp_cat"}].


%%-----------------------------------------------------------------------------
%% @doc Checks if an object is registered in a naming context.
%% @spec object_is_regged(NC, Name) -> true | false
%% where
%%      NC = object_ref()
%%          The naming context to begin looking under.
%%      Name = [#'CosNaming_NameComponent'{}]
%% @end
-spec(object_is_regged(NC::object_ref(),
        Name::[#'CosNaming_NameComponent'{}]) -> true | false).
%%-----------------------------------------------------------------------------
object_is_regged(_, []) ->
    false;
object_is_regged(NC, [H]) ->
    {ok, BList, ?ORBER_NIL_OBJREF} = 'CosNaming_NamingContext':list(NC, 10),
    lists:any(fun(B) -> is_obj(H, B) end, BList);
object_is_regged(NC, [H|T]) ->
    {ok, BList, ?ORBER_NIL_OBJREF} = 'CosNaming_NamingContext':list(NC, 10),
    % There will not be more than 10 objects during testing, so no need to
    % check the remainder of the list.
    case lists:filter(fun(B) -> is_nc(H, B) end, BList)
        of [#'CosNaming_Binding'{binding_name=Name}] ->
            NextNC = 'CosNaming_NamingContext':resolve(NC, Name),
            object_is_regged(NextNC, T)
         ; _ ->
            false
    end.

is_obj(Name, #'CosNaming_Binding'{binding_name=[Name],
        binding_type=nobject}) ->
    true;
is_obj(_, _) ->
    false.

is_nc(Name, #'CosNaming_Binding'{binding_name=[Name],
        binding_type=ncontext}) ->
    true;
is_nc(_, _) ->
    false.

-endif. % TEST

