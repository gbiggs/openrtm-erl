%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Configuration functionality.
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
-module(config).


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
-export([empty_conf/0, get_value/3, get_value/2, set_value/3, read_conf_file/1,
        merge/2, tokens/1, tokens/2, tokens/3, val_to_str/1, flatten/1,
        unflatten/1, compare/2, compare/4, compare_split/3]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Creates a new empty configuration.
%% @spec empty_conf() -> config()
%% @end
-spec(empty_conf() -> config()).
%%-----------------------------------------------------------------------------
empty_conf() ->
    [].


%%-----------------------------------------------------------------------------
%% @doc Gets a value from a configuration, using a provided default if the
%% value is not defined.
%% @spec get_value(Key, Default, config()) -> string() | undefined
%% where
%%      Key = string() | [string()]
%%      Default = string()
%% @end
-spec(get_value(Key::string() | [string()], Default::string(), C::config()) ->
        Value::string()).
%%-----------------------------------------------------------------------------
get_value(Key, Default, C) ->
    case get_value(Key, C)
        of undefined ->
            Default
         ; Value ->
            Value
    end.


%%-----------------------------------------------------------------------------
%% @doc Gets a value from a configuration.
%% @spec get_value(Key, config()) -> string() | undefined
%% where
%%      Key = string() | [string()]
%% @end
-spec(get_value(Key::string() | [string()], C::config()) ->
        Value::string() | undefined).
%%-----------------------------------------------------------------------------
get_value([H|_]=Key, C) when is_list(H) ->
    % This is a key pointing to a value in a sub-config.
    get_value1(Key, C);
get_value(Key, C) ->
    proplists:get_value(Key, C).

get_value1([H], C) ->
    proplists:get_value(H, C);
get_value1([H|T], C) ->
    case proplists:get_value(H, C)
        of undefined ->
            undefined
         ; C1 ->
            get_value1(T, C1)
    end.


%%-----------------------------------------------------------------------------
%% @doc Sets a value in a configuration.
%% @spec set_value(Key, Value, config()) -> config()
%% where
%%      Key = string() | [string()]
%%      Value = string() | [string() | tuple()]
%% @end
-spec(set_value(Key::string() | [string()],
        Value::string() | [string() | tuple()], C::config()) ->
    config()).
%%-----------------------------------------------------------------------------
set_value([H|T]=Key, Value, C) when is_list(Key), is_list(H) ->
    % Key is a list of strings
    merge(C, [{H, tuplise(lists:reverse(T), Value)}]);
set_value(Key, Value, C) ->
    merge(C, [{Key, Value}]).


%%-----------------------------------------------------------------------------
%% @doc Turns a list of keys and a value into their config representation. The
%% list of keys should be reversed before calling this function.
%% @spec tuplise(Key, Value) -> config()
%% where
%%      Key = [string()]
%%      Value = string() | [string() | tuple()]
%% @end
-spec(tuplise(Key::[string()], Value::string() | [string() | tuple()]) ->
    config()).
%%-----------------------------------------------------------------------------
tuplise([], Acc) ->
    Acc;
tuplise([H|T], Acc) ->
    tuplise(T, [{H, Acc}]).


%%-----------------------------------------------------------------------------
%% @doc Loads a configuration file and reads it into a tree of configs.
%% @spec read_conf_file(Fd) -> {ok, config()}
%% where
%%      Fd = pid()
%% @todo Return some error values when things go wrong.
%% @end
-spec(read_conf_file(Fd::pid()) -> {ok, config()}).
%%-----------------------------------------------------------------------------
read_conf_file(Fd) ->
    {ok, FN} = file:pid2name(Fd),
    ?LOG(rtl_info, "Reading configuration file ~s", [FN]),
    {ok, read_conf_file1(get_comp_line(Fd), Fd, [])}.


%%-----------------------------------------------------------------------------
%% @doc Merges the names and values from C2 into C1. Any duplicate
%% names are overwritten with the values from C2.
%% @spec merge(C1, C2) -> C3
%% where
%%      C1 = config()
%%      C2 = config()
%%      C3 = config()
%% @end
-spec(merge(C1::config(), C2::config()) -> config()).
%%-----------------------------------------------------------------------------
merge(C1, C2) ->
    lists:foldl(fun merge_val_list/2, C1, C2).


%%-----------------------------------------------------------------------------
%% @doc Equivalent to calling tokens(String, ",", both).
%% @spec tokens(String) -> [string()]
%% where
%%      String = string()
%% @end
-spec(tokens(String::string()) -> [string()]).
%%-----------------------------------------------------------------------------
tokens(S) ->
    tokens(S, ",", both).


%%-----------------------------------------------------------------------------
%% @doc Equivalent to calling tokens(String, Separators, both).
%% @spec tokens(String, Separators) -> [string()]
%% where
%%      String = string()
%%      Separators = string()
%% @end
-spec(tokens(String::string(), Separators::string()) -> [string()]).
%%-----------------------------------------------------------------------------
tokens(S, T) ->
    tokens(S, T, both).


%%-----------------------------------------------------------------------------
%% @doc Tokenises a string separated by the specified separator characters.
%% Differs from string::tokens/2 in that it also strips the resulting tokens
%% of whitespace at either the left, right or both sides.
%% @spec tokens(String, Separators, Dir) -> [string()]
%% where
%%      String = string()
%%      Separators = string()
%%      Dir = left | right | both
%% @end
-spec(tokens(String::string(), Separators::string(),
        Dir::left | right | both) -> [string()]).
%%-----------------------------------------------------------------------------
tokens(S, Seps, Dir) ->
    Tokens = string:tokens(S, Seps),
    Tokens1 = lists:map(fun(T) -> string:strip(T, Dir, $ ) end, Tokens),
    lists:map(fun(T) -> string:strip(T, Dir, $\t) end, Tokens1).


%%-----------------------------------------------------------------------------
%% @doc Turn a value into a string: string -> string, boolean -> string,
%% undefined -> ""
%% @spec val_to_str(Val) -> string()
%% where
%%      Val = string() | true | false | undefined
%% @end
-spec(val_to_str(Val::string() | true | false | undefined) -> string()).
%%-----------------------------------------------------------------------------
val_to_str(true) ->
    "YES";
val_to_str(false) ->
    "NO";
val_to_str(undefined) ->
    "";
val_to_str(Val) ->
    Val.


%%-----------------------------------------------------------------------------
%% @doc Flatten a config to a list of tuples.
%% @spec flatten(config()) -> [{string(), string()}]
%% @end
-spec(flatten(config()) -> [{string(), string()}]).
%%-----------------------------------------------------------------------------
flatten(C) ->
    flatten_params(flatten1(C, []), []).

-spec(flatten1(config(), [{[string()], string()}]) ->
    [{[string()], string()}]).
flatten1([], Acc) ->
    Acc;
flatten1([{P, [{_, _}|_]=V} | T], Acc) ->
    flatten1(T, duplicate(P, flatten1(V, []), []) ++ Acc);
flatten1([{P, V} | T], Acc) ->
    flatten1(T, [{[P], V} | Acc]).

-spec(duplicate(string(), [{[string()], string()}],
        [{[string()], string()}]) ->
    [{[string()], string()}]).
duplicate(_, [], Acc) ->
    Acc;
duplicate(P, [{P1, V} | T], Acc) ->
    duplicate(P, T, [{[P | P1], V} | Acc]).

-spec(flatten_params([{[string()], string()}], [{string(), string()}]) ->
    [{string(), string()}]).
flatten_params([], Acc) ->
    Acc;
flatten_params([{P, V} | T], Acc) ->
    flatten_params(T, [{string:join(P, "."), V} | Acc]).


%%-----------------------------------------------------------------------------
%% @doc Unflatten a config from a list of tuples.
%% @spec unflatten([{string(), string()}]) -> config()
%% @end
-spec(unflatten([{string(), string()}]) -> config()).
%%-----------------------------------------------------------------------------
unflatten(L) ->
    unflatten1(L, []).

-spec(unflatten1([{string(), string()}], config()) -> config()).
unflatten1([], C) ->
    C;
unflatten1([{K, V}|T], C) ->
    unflatten1(T, add_prop(proplists:property({string:tokens(K, "."), V}), C)).


%%-----------------------------------------------------------------------------
%% @doc Compare two configs, ignoring missing values and splitting parameters.
%% @spec compare(config(), config()) -> boolean()
%% @see compare/4
%% @end
-spec(compare(C1::config(), C2::config()) -> boolean()).
%%-----------------------------------------------------------------------------
compare(C1, C2) ->
    compare(C1, C2, true, true).


%%-----------------------------------------------------------------------------
%% @doc Compare two configs, possibly ignoring missing values and/or splitting
%% parameters.
%% @spec compare(config(), config(), Ignore, Split) -> boolean()
%% where
%%      Ignore = boolean()
%%          If a parameter appears in one set but not in the other, ignore it.
%%      Split = boolean()
%%          If a parameter is splittable, split it and accept a match if at
%%          least one element from each set is in the other.
%% @end
-spec(compare(C1::config(), C2::config(), Ignore::boolean(),
        Split::boolean()) ->
    boolean()).
%%-----------------------------------------------------------------------------
compare(C1, C2, Ignore, Split) ->
    FlatC1 = flatten(C1),
    FlatC2 = flatten(C2),
    compare1(FlatC1, FlatC2, Ignore, Split).

-spec(compare1(C1::proplist(), C2::proplist(), Ignore::boolean(),
        Split::boolean()) ->
    boolean()).
compare1(C1, C2, true, Split) ->
    C1F = fun({X, _}) -> proplists:is_defined(X, C2) end,
    C2F = fun({X, _}) -> proplists:is_defined(X, C1) end,
    compare2(lists:filter(C1F, C1), lists:filter(C2F, C2), Split);
compare1(C1, C2, false, Split) ->
    compare2(C1, C2, Split).

-spec(compare2(C1::proplist(), C2::proplist(), Split::boolean()) -> boolean()).
compare2(C1, C2, Split) ->
    FGen = fun(L) ->
        fun({P, V}) ->
            case proplists:get_value(P, L)
                of undefined ->
                    true
                 ; OtherV ->
                    not params_match(V, OtherV, Split)
            end
        end
    end,
    case {lists:filter(FGen(C2), C1), lists:filter(FGen(C1), C2)}
        of {[], []} ->
            true
         ; Other ->
            ?LOG(rtl_debug, "Comparison found unmatched values: ~p", [Other]),
            false
    end.


-spec(params_match(P1::string(), P2::string(), Split::boolean()) -> boolean()).
params_match(P, P, _) ->
    true;
params_match(_P1, _P2, false) ->
    false;
params_match(P1, P2, true) ->
    SplitP1 = tokens(P1),
    SplitP2 = tokens(P2),
    has_at_least_one(SplitP1, SplitP2).

-spec(has_at_least_one(P1::[string()], P2::[string()]) -> boolean()).
has_at_least_one([], _P2) ->
    false;
has_at_least_one([H|T], P2) ->
    case lists:member(H, P2)
        of true ->
            true
         ; false ->
            has_at_least_one(T, P2)
    end.



%%-----------------------------------------------------------------------------
%% @doc Compare two configs, possibly ignoring missing values.
%% @spec compare_split(config(), config(), Ignore) -> boolean()
%% where
%%      Ignore = boolean()
%% @see compare/4
%% @end
-spec(compare_split(C1::config(), C2::config(), Ignore::boolean()) ->
    boolean()).
%%-----------------------------------------------------------------------------
compare_split(C1, C2, Ignore) ->
    compare(C1, C2, Ignore, true).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Merges a single property into a property list. If the key already
%% exists in the list, the new value overwrites the existing value, unless both
%% are themselves configs. In this case, the two configs are merged
%% recursively. Normal list values are treated as immutable rather than merged.
%% @spec merge_val_list({K, V}, C) -> C1
%% where
%%      K = any()
%%      V = any()
%%      C = config()
%%      C1 = config()
%% @end
-spec(merge_val_list({K::any(), V::any()}, C::config()) -> config()).
%%-----------------------------------------------------------------------------
merge_val_list({K, V}=Prop, C) ->
    case proplists:is_defined(K, C)
        of true ->
            % Merge the values
            NewV = merge_vals(proplists:get_value(K, C), V),
            lists:sort([proplists:property(K, NewV) | proplists:delete(K, C)])
         ; false ->
            lists:sort([Prop | C])
    end.

%%-----------------------------------------------------------------------------
%% @doc Merges two values. If both values are configs, they are merged
%% recursively using merge/2. All other values are treated as immutable, and
%% the RHS value overwrites the LHS value.
%% @spec merge_vals(LHS, RHS) -> V
%% where
%%      LHS = any()
%%      RHS = any()
%%      V = any()
%% @end
-spec(merge_vals(LHS::any(), RHS::any()) -> any()).
%%-----------------------------------------------------------------------------
merge_vals(LHS, RHS) when not(is_list(LHS)) or not(is_list(RHS)) ->
    RHS;
merge_vals([{_, _}|_]=LHS, [{_, _}|_]=RHS) ->
    % Both LHS and RHS are probably property lists, so merge them recursively.
    merge(LHS, RHS);
merge_vals(_LHS, RHS) ->
    RHS.


%%-----------------------------------------------------------------------------
%% @doc Reads a complete line from the configuration file. This means following
%% continuation characters to the next line, removing leading white space, etc.
%% @spec get_comp_line(Fd) -> string() | eof | trailing_line_continuation
%% where
%%      Fd = pid()
%% @end
-spec(get_comp_line(Fd::pid()) -> string() | eof | trailing_line_continuation).
%%-----------------------------------------------------------------------------
get_comp_line(Fd) ->
    get_comp_line1(read_next_line(Fd), Fd, []).

get_comp_line1(eof, _Fd, []) ->
    eof;
get_comp_line1(eof, _Fd, Acc) ->
    merge_lines(lists:reverse(Acc));
get_comp_line1(L, Fd, Acc) ->
    case lists:last(L)
        of $\\ ->
            NextL = read_next_line(Fd),
            % This case is necessary to catch trailing line continuations.
            case NextL
                of eof ->
                    trailing_line_continuation
                 ; _ ->
                    get_comp_line1(NextL, Fd, [L | Acc])
            end
         ; _ ->
            merge_lines(lists:reverse([L | Acc]))
    end.


%%-----------------------------------------------------------------------------
%% @doc Reads the next line from the file. Comment lines are ignored.
%% @spec read_next_line(Fd) -> string() | eof
%% where
%%      Fd = pid()
%% @end
-spec(read_next_line(Fd::pid()) -> string() | eof).
%%-----------------------------------------------------------------------------
read_next_line(Fd) ->
    read_next_line1(file:read_line(Fd), Fd).

read_next_line1(eof, _Fd) ->
    eof;
read_next_line1({ok, L}, Fd) ->
    case is_comment(L)
        of true ->
            read_next_line(Fd)
         ; false ->
            string:strip(L, right, $\n)
    end.


%%-----------------------------------------------------------------------------
%% @doc Tests if a line is a comment.
%% @spec is_comment(string()) -> boolean()
%% @end
-spec(is_comment(Line::string()) -> boolean()).
%%-----------------------------------------------------------------------------
is_comment([$#|_]) ->
    true;
is_comment([$!|_]) ->
    true;
is_comment(_) ->
    false.


%%-----------------------------------------------------------------------------
%% @doc Merges a list of lines, stripping continuation characters from the end
%% and white space from the beginning.
%% @spec merge_lines([string()]) -> string()
%% @end
-spec(merge_lines([string()]) -> string()).
%%-----------------------------------------------------------------------------
merge_lines([]) ->
    [];
merge_lines([H|T]) ->
    %lists:foldl(fun(L, Acc) -> Acc ++ clean_line(L) end, H, T).
    merge_lines1(T, clean_line(H)).

merge_lines1([], L) ->
    L;
merge_lines1([H|T], L) ->
    merge_lines1(T, L ++ clean_line(H)).


%%-----------------------------------------------------------------------------
%% @doc Cleans line continuation characters from the end of a line and white
%% space from the beginning.
%% @spec clean_line(string()) -> string()
%% @end
-spec(clean_line(string()) -> string()).
%%-----------------------------------------------------------------------------
clean_line(L) ->
    F = fun(C) when (C =:= $\t) or (C =:= $ ) -> true; (_) -> false end,
    L1 = lists:dropwhile(F, L),
    string:strip(L1, right, $\\).


%%-----------------------------------------------------------------------------
%% @doc Turns a line into a name/value pair. The name is list of names, if
%% it has more than one member, this indicates that it refers to
%% sub-config entries.
%% @spec line_to_prop(string()) -> {[string()], string()}
%% @end
-spec(line_to_prop(Line::string()) -> {[string()], string()}).
%%-----------------------------------------------------------------------------
line_to_prop(L) ->
    line_to_prop1(re:run(L, "^\s*([\\w\\d\\.]+)\\s*(?::|=)?\\s*(.*)",
            [{newline, any}, unicode, {capture, all_but_first, list}])).

line_to_prop1({match, [Name, []]}) ->
    proplists:property({string:tokens(Name, "."), true});
line_to_prop1({match, [Name, Value]}) ->
    proplists:property({string:tokens(Name, "."), Value}).


%%-----------------------------------------------------------------------------
%% @doc Splits a name into individual parts separated by dots, but
%% only if necessary.
%% @spec split_name(string()) -> string() | [string()]
%% @end
-spec(split_name(P::string()) -> string() | [string()]).
%%-----------------------------------------------------------------------------
split_name(P) ->
    case string:tokens(P, ".")
        of [T] ->
            T
         ; Tokens ->
            Tokens
    end.


%%-----------------------------------------------------------------------------
%% @doc Reads lines into a config tree.
%% @spec read_conf_file1(L, Fd, Acc) -> config()
%% where
%%      L = string()
%%      Fd = pid()
%%      Acc = config()
%% @end
-spec(read_conf_file1(L::string() | atom(), Fd::pid(), Acc::config()) ->
        config()).
%%-----------------------------------------------------------------------------
read_conf_file1(eof, _Fd, Acc) ->
    Acc;
read_conf_file1(L, Fd, Acc) when is_list(L) ->
    Prop = line_to_prop(L),
    Acc2 = add_prop(Prop, Acc),
    read_conf_file1(get_comp_line(Fd), Fd, Acc2).


%%-----------------------------------------------------------------------------
%% @doc Adds a property to a config, creating nested configs as necessary.
%% @spec add_prop(Prop, config()) -> config()
%% where
%%      Prop = {[string()], string()}
%% @end
-spec(add_prop(Prop::{[string()], string()}, C::config()) ->
        config()).
%%-----------------------------------------------------------------------------
add_prop({Names, V}, C) when is_list(Names) ->
    % Create a new config tree for the names
    NewProp = create_prop(Names, V),
    merge_val_list(NewProp, C).


%%-----------------------------------------------------------------------------
%% @doc Turns a property into the format suitable for the tree configs,
%% creating a set of concentric configs around a single value if necessary.
%% @spec create_prop(Names, Value) -> configitem()
%% where
%%      Names = [string()]
%%      Value = string()
%% @end
-spec(create_prop([string()], string()) -> configitem()).
%%-----------------------------------------------------------------------------
create_prop([Name], Value) ->
    {Name, Value};
create_prop([H|T], Value) ->
    {H, [create_prop(T, Value)]}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test merging a single value into a config.
%% @spec merge_val_list_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(merge_val_list_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
merge_val_list_test_() ->
    [?_assertMatch([{name, value}], merge_val_list({name, value}, [])),
        ?_assertMatch([{name, value1}],
            merge_val_list({name, value1}, [{name, value2}])),
        ?_assertMatch([{name1, value1}, {name2, value2}],
            merge_val_list({name1, value1}, [{name2, value2}])),
        ?_assertMatch([{name, [value1, value2]}],
            merge_val_list({name, [value1, value2]}, [{name, value3}])),
        ?_assertMatch([{name, [value1, value2]}, {name2, value3}],
            merge_val_list({name, [value1, value2]}, [{name2, value3}]))].


%%-----------------------------------------------------------------------------
%% @doc Test merging values.
%% @spec merge_vals_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(merge_vals_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
merge_vals_test_() ->
    [?_assertMatch(value2, merge_vals(value1, value2)),
        ?_assertMatch(value2, merge_vals([{name, value}], value2)),
        ?_assertMatch([{name, value}], merge_vals(value1, [{name, value}])),
        ?_assertMatch([{name, value3}],
            merge_vals([value1, value2], [{name, value3}])),
        ?_assertMatch([value2, value3], merge_vals(value1, [value2, value3])),
        ?_assertMatch([value2, value3],
            merge_vals([{name, value}], [value2, value3])),
        ?_assertMatch([{name, value2}],
            merge_vals([{name, value1}], [{name, value2}])),
        ?_assertMatch([{name1, value1}, {name2, value2}],
            merge_vals([{name1, value1}], [{name2, value2}])),
        ?_assertMatch([{name1, value1},
                {name2, [{name3, value3}, {name4, true}]}],
            merge_vals([{name1, value1}, {name2, [{name3, value3}]}],
                [{name2, [{name4, true}]}]))].


%%-----------------------------------------------------------------------------
%% @doc Test getting a complete line from a file: single line file.
%% @spec get_comp_line_single_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_comp_line_single_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_comp_line_single_test_() ->
    {setup,
        fun() -> setup_file("test_conf_file",
                    ["single line", "next line"]) end,
        fun(S) -> cleanup_file(S) end,
        fun({_, Fd}) ->
                ?_assertMatch("single line", get_comp_line(Fd))
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test getting a complete line from a file: two lines, no whitespace.
%% @spec get_comp_line_2nws_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_comp_line_2nws_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_comp_line_2nws_test_() ->
    {setup,
        fun() ->
            setup_file("test_conf_file", ["continuing\\", "line", "next line"])
        end,
        fun(S) -> cleanup_file(S) end,
        fun({_, Fd}) ->
                ?_assertMatch("continuingline", get_comp_line(Fd))
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test getting a complete line from a file: two lines, leading
%% whitespace.
%% @spec get_comp_line_2ws_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_comp_line_2ws_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_comp_line_2ws_test_() ->
    {setup,
        fun() ->
            setup_file("test_conf_file",
                ["white space \\", "   line", "next line"])
        end,
        fun(S) -> cleanup_file(S) end,
        fun({_, Fd}) ->
                ?_assertMatch("white space line", get_comp_line(Fd))
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Unit test support function that sets up a file with lines of text.
%% @spec setup_file(FN, Lines) -> State
%% where
%%      FN = string()
%%      Lines = [string()]
%%      State = {FN, Fd}
%%      Fd = pid()
%% @end
-spec(setup_file(FN::string(), Lines::[string()]) ->
    {FN::string(), Fd::pid()}).
%%-----------------------------------------------------------------------------
setup_file(FN, Lines) ->
    {ok, Fd} = file:open(FN, [write]),
    lists:foreach(fun(L) -> io:fwrite(Fd, "~s~n", [L]) end, Lines),
    ok = file:close(Fd),
    {ok, Fd2} = file:open(FN, [read]),
    {FN, Fd2}.


%%-----------------------------------------------------------------------------
%% @doc Unit test support function that cleans up a file.
%% @spec cleanup_file({FN, Fd}) -> ok
%% where
%%      FN = string()
%%      Fd = pid()
%% @end
-spec(cleanup_file({FN::string(), Fd::pid()}) -> ok).
%%-----------------------------------------------------------------------------
cleanup_file({FN, Fd}) ->
    ok = file:close(Fd),
    ok = file:delete(FN).


%%-----------------------------------------------------------------------------
%% @doc Test comment checks.
%% @spec is_comment_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(is_comment_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
is_comment_test_() ->
    [?_assertMatch(true, is_comment("#stuff")),
        ?_assertMatch(true, is_comment("!stuff")),
        ?_assertMatch(false, is_comment("stuff")),
        ?_assertMatch(false, is_comment(" #stuff")),
        ?_assertMatch(false, is_comment(" !stuff"))].


%%-----------------------------------------------------------------------------
%% @doc Test merging lines.
%% @spec merge_lines_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(merge_lines_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
merge_lines_test_() ->
    [?_assertMatch([], merge_lines([])),
        ?_assertMatch("this is line 1.", merge_lines(["this is line 1."])),
        ?_assertMatch("this is line 2.", merge_lines(["this i\\", "s line 2."])),
        ?_assertMatch("this is line 3.",
            merge_lines(["this is \\", "line 3."])),
        ?_assertMatch("this isline 4.", merge_lines(["this is\\", " line 4."])),
        ?_assertMatch("this is line 5.",
            merge_lines(["this \\", "is \\", " line 5."]))].


%%-----------------------------------------------------------------------------
%% @doc Test cleaning lines.
%% @spec clean_lines_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(clean_lines_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
clean_lines_test_() ->
    [?_assertMatch("line", clean_line("line")),
        ?_assertMatch("line line", clean_line("line line")),
        ?_assertMatch("line line2", clean_line(" line line2")),
        ?_assertMatch("line line3", clean_line("\tline line3")),
        ?_assertMatch("line line4", clean_line(" \tline line4")),
        ?_assertMatch("line line5", clean_line("\t line line5")),
        ?_assertMatch("line line6", clean_line("line line6\\")),
        ?_assertMatch("line line7 ", clean_line("line line7 \\")),
        ?_assertMatch("line line8", clean_line(" line line8\\")),
        ?_assertMatch("line line9 ", clean_line(" line line9 \\")),
        ?_assertMatch("line line10", clean_line("\tline line10\\")),
        ?_assertMatch("line line11\t", clean_line("\tline line11\t\\")),
        ?_assertMatch("line line12", clean_line(" \tline line12\\")),
        ?_assertMatch("line line13 \t", clean_line(" \tline line13 \t\\")),
        ?_assertMatch("line line14", clean_line("\t line line14\\")),
        ?_assertMatch("line line15 ", clean_line("\t line line15 \\"))].


%%-----------------------------------------------------------------------------
%% @doc Test converting a string to a property.
%% @spec line_to_prop_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(line_to_prop_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
line_to_prop_test_() ->
    [?_assertMatch({["name"], "value"}, line_to_prop("name=value")),
        ?_assertMatch({["name"], "value"}, line_to_prop("name: value")),
        ?_assertMatch({["name"], "value"}, line_to_prop("name value")),
        ?_assertMatch({["name"], "value"}, line_to_prop("name = value")),
        ?_assertMatch({["name"], "value"}, line_to_prop("name : value")),
        ?_assertMatch({["name"], "value"}, line_to_prop("name   value")),
        ?_assertMatch({["name"], "value"}, line_to_prop("name:   value")),
        ?_assertMatch({["name"], "value"}, line_to_prop("name=   value")),
        ?_assertMatch({["name"], "value"}, line_to_prop("name=   value")),
        ?_assertMatch({["name"], "value   "},
            line_to_prop("name=   value   ")),
        ?_assertMatch({["name"], "value   "},
            line_to_prop("name  =   value   ")),
        ?_assertMatch({["name"], "value value"},
            line_to_prop("name value value")),
        ?_assertMatch({["name"], "value value"},
            line_to_prop("name = value value")),
        ?_assertMatch({["name"], "value value"},
            line_to_prop("name : value value")),
        ?_assertMatch({["lvl1", "lvl2"], "value"},
            line_to_prop("lvl1.lvl2: value")),
        ?_assertMatch({["lvl1", "lvl2"], "value"},
            line_to_prop("lvl1.lvl2 value")),
        ?_assertMatch({["lvl1", "lvl2", "lvl3"], "value"},
            line_to_prop("lvl1.lvl2.lvl3 = value")),
        ?_assertMatch({["name"], true}, line_to_prop("name")),
        ?_assertMatch({["name"], true}, line_to_prop("name:")),
        ?_assertMatch({["name"], true}, line_to_prop("name=")),
        ?_assertMatch({["lvl1", "lvl2"], true}, line_to_prop("lvl1.lvl2"))].


%%-----------------------------------------------------------------------------
%% @doc Test splitting names.
%% @spec split_name_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(split_name_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
split_name_test_() ->
    [?_assertMatch("name", split_name("name")),
        ?_assertMatch(["blurgle", "splurt"], split_name("blurgle.splurt")),
        ?_assertMatch(["blurgle", "splurt", "name"],
            split_name("blurgle.splurt.name"))].


%%-----------------------------------------------------------------------------
%% @doc Test adding a property to a config.
%% @spec add_prop_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(add_prop_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
add_prop_test_() ->
    [?_assertMatch([{"name", "value"}], add_prop({["name"], "value"}, [])),
        ?_assertMatch([{"name1", "value1"}, {"name2", "value2"}],
            add_prop({["name1"], "value1"}, [{"name2", "value2"}])),
        ?_assertMatch([{"name1", [{"name3", "value1"}]}, {"name2", "value2"}],
            add_prop({["name1", "name3"], "value1"}, [{"name2", "value2"}]))].


%%-----------------------------------------------------------------------------
%% @doc Test creating sub-lists based on lists of names.
%% @spec create_prop_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(create_prop_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
create_prop_test_() ->
    [?_assertMatch({"name", "value"}, create_prop(["name"], "value")),
        ?_assertMatch({"name1", [{"name2", "value"}]},
            create_prop(["name1", "name2"], "value")),
        ?_assertMatch({"name1", [{"name2", [{"name3", "value"}]}]},
            create_prop(["name1", "name2", "name3"], "value"))].

%%-----------------------------------------------------------------------------
%% @doc Test converting a value into a string.
%% @spec val_to_str_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(val_to_str_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
val_to_str_test_() ->
    [?_assertMatch("YES", val_to_str(true)),
        ?_assertMatch("NO", val_to_str(false)),
        ?_assertMatch("", val_to_str(undefined)),
        ?_assertMatch("true", val_to_str("true")),
        ?_assertMatch("false", val_to_str("false")),
        ?_assertMatch("blurgle", val_to_str("blurgle"))].

-endif. % TEST

