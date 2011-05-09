%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Queue-based buffer implementation.
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
-module(queue_buffer).


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
%% Records
%%-----------------------------------------------------------------------------
-record(buf, {size :: pos_integer(), % Maximum size of the buffer
        in=[] :: [any()], % In queue
        out=[] :: [any()] % Out queue
    }).


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([new/1, delete/1, write/2, read/1, count/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create a new buffer.
%% @spec new(Config) -> #buf{} | {error, Reason}
%% where
%%      Config = proplist()
%%          Configuration of the buffer. Allowable values are size (the
%%          maximum size of the buffer, default is 10).
%%      Reason = any()
%% @end
-spec(new(Config::proplist()) -> #buf{} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
new(Config) ->
    create_buffer(proplists:get_value("size", Config, 10)).


%%-----------------------------------------------------------------------------
%% @doc Delete a buffer
%% @spec delete(Buffer) -> ok
%% where
%%      Buffer = #buf{}
%% @end
-spec(delete(#buf{}) -> ok).
%%-----------------------------------------------------------------------------
delete(_Buf) ->
    ok.


%%-----------------------------------------------------------------------------
%% @doc Write to the buffer.
%% @spec write(Value, Buffer) -> {ok | overflow | {error, Reason}, NewBuffer}
%% where
%%      Value = any()
%%      Buffer = #buf{}
%%      Reason = any()
%%      NewBuffer = #buf{}
%% @end
-spec(write(Value::any(), Buffer::#buf{}) ->
    {{ok | overflow | {error, Reason::any()}}, NewBuffer::#buf{}}).
%%-----------------------------------------------------------------------------
write(Value, #buf{size=Size, in=In, out=Out}) ->
    NewBuf = case length(In) + length(Out)
        of Size ->
            Result = overflow,
            {_, NewIn, NewOut} = pop_one(In, Out),
            #buf{size=Size, in=[Value|NewIn], out=NewOut}
         ; _ ->
            Result = ok,
            NewIn = [Value|In],
            NewOut = Out,
            #buf{size=Size, in=NewIn, out=NewOut}
    end,
    {Result, NewBuf}.


%%-----------------------------------------------------------------------------
%% @doc Read from the buffer.
%% @spec read(Buffer) -> {Value | {error, empty | Reason}, NewBuffer}
%% where
%%      Buffer = #buf{}
%%      Value = any()
%%      Reason = any()
%%      NewBuffer = #buf{}
%% @end
-spec(read(Buffer::#buf{}) ->
    {{Value::any() | {error, empty | any()}}, NewBuffer::#buf{}}).
%%-----------------------------------------------------------------------------
read(#buf{in=In, out=Out}=Buf) ->
    {Value, NewIn, NewOut} = pop_one(In, Out),
    {Value, Buf#buf{in=NewIn, out=NewOut}}.


%%-----------------------------------------------------------------------------
%% @doc Count the number of items in the buffer.
%% @spec count(Buffer) -> {pos_integer(), NewBuffer}
%% where
%%      Buffer = #buf{}
%%      NewBuffer = #buf{}
%% @end
-spec(count(Buffer::#buf{}) -> {pos_integer(), NewBuffer::#buf{}}).
%%-----------------------------------------------------------------------------
count(#buf{in=In, out=Out}=Buf) ->
    {length(In) + length(Out), Buf}.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create a buffer.
%% @spec create_buffer(Size) -> #buf{} | {error, Reason}
%% where
%%      Size = pos_integer()
%%      Reason = any()
%% @end
-spec(create_buffer(Size::pos_integer()) -> #buf{} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
create_buffer(Size) when Size =< 0 ->
    {error, bad_size};
create_buffer(Size) ->
    #buf{size=Size}.


%%-----------------------------------------------------------------------------
%% @doc Pop a single value off the dual lists.
%% @spec pop_one(In, Out) -> {Value, NewIn, NewOut}
%% where
%%      In = [any()]
%%      Out = [any()]
%%      Value = any() | {error, empty}
%%      NewIn = [any()]
%%      NewOut = [any()]
%% @end
-spec(pop_one(In::[any()], Out::[any()]) ->
    {Value::any() | {error, empty}, NewIn::[any()], NewOut::[any()]}).
%%-----------------------------------------------------------------------------
pop_one([], []) ->
    {{error, empty}, [], []};
pop_one([], [X|Rest]) ->
    {X, [], Rest};
pop_one(In, []) ->
    pop_one([], lists:reverse(In));
pop_one(In, [X|Rest]) ->
    {X, In, Rest}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test writing to a buffer.
%% @spec write_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(write_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
write_test_() ->
    [?_assertMatch({ok, #buf{in=[1], out=[]}}, write(1, #buf{size=3})),
        ?_assertMatch({ok, #buf{in=[2,1], out=[]}},
            write(2, #buf{size=3, in=[1]})),
        ?_assertMatch({ok, #buf{in=[3,2,1], out=[]}},
            write(3, #buf{size=3, in=[2,1]})),
        ?_assertMatch({overflow, #buf{in=[4], out=[2,3]}},
            write(4, #buf{size=3, in=[3,2,1]})),
        ?_assertMatch({overflow, #buf{in=[4], out=[2,3]}},
            write(4, #buf{size=3, in=[], out=[1,2,3]})),
        ?_assertMatch({overflow, #buf{in=[4,3], out=[2]}},
            write(4, #buf{size=3, in=[3], out=[1,2]})),
        ?_assertMatch({ok, #buf{in=[4,3], out=[2]}},
            write(4, #buf{size=3, in=[3], out=[2]}))].


%%-----------------------------------------------------------------------------
%% @doc Test reading from a buffer.
%% @spec read_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(read_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
read_test_() ->
    [?_assertMatch({{error, empty}, #buf{size=3}}, read(#buf{size=3})),
        ?_assertMatch({1, #buf{size=3, out=[]}}, read(#buf{size=3, out=[1]})),
        ?_assertMatch({1, #buf{size=3, out=[2]}},
            read(#buf{size=3, out=[1,2]})),
        ?_assertMatch({1, #buf{size=3, out=[2,3]}},
            read(#buf{size=3, out=[1,2,3]})),
        ?_assertMatch({1, #buf{size=3, in=[], out=[]}},
            read(#buf{size=3, in=[1]})),
        ?_assertMatch({1, #buf{size=3, out=[2]}},
            read(#buf{size=3, in=[2,1]})),
        ?_assertMatch({1, #buf{size=3, out=[2,3]}},
            read(#buf{size=3, in=[3,2,1]})),
        ?_assertMatch({1, #buf{size=3, in=[3], out=[2]}},
            read(#buf{size=3, in=[3], out=[1,2]})),
        ?_assertMatch({1, #buf{size=3, in=[3,2], out=[]}},
            read(#buf{size=3, in=[3,2], out=[1]}))].


%%-----------------------------------------------------------------------------
%% @doc Test counting the number of items in a buffer.
%% @spec count_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(count_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
count_test_() ->
    [?_assertMatch({0, #buf{size=3}}, count(#buf{size=3})),
        ?_assertMatch({1, #buf{size=3, in=[1]}}, count(#buf{size=3, in=[1]})),
        ?_assertMatch({2, #buf{size=3, in=[1, 2]}},
            count(#buf{size=3, in=[1, 2]})),
        ?_assertMatch({3, #buf{size=3, in=[1, 2, 3]}},
            count(#buf{size=3, in=[1, 2, 3]})),
        ?_assertMatch({3, #buf{size=3, in=[2, 3], out=[1]}},
            count(#buf{size=3, in=[2, 3], out=[1]})),
        ?_assertMatch({3, #buf{size=3, in=[3], out=[1, 2]}},
            count(#buf{size=3, in=[3], out=[1, 2]})),
        ?_assertMatch({3, #buf{size=3, in=[], out=[1, 2, 3]}},
            count(#buf{size=3, in=[], out=[1, 2, 3]})),
        ?_assertMatch({2, #buf{size=3, in=[], out=[2, 3]}},
            count(#buf{size=3, in=[], out=[2, 3]})),
        ?_assertMatch({1, #buf{size=3, in=[], out=[3]}},
            count(#buf{size=3, in=[], out=[3]}))].


%%-----------------------------------------------------------------------------
%% @doc Test creating a new buffer.
%% @spec new_test() -> ok
%% @end
-spec(new_test() -> ok).
%%-----------------------------------------------------------------------------
new_test() ->
    ?assertMatch(#buf{size=10, in=[], out=[]}, new([])),
    ?assertMatch(#buf{size=5, in=[], out=[]}, new([{"size", 5}])),
    ?assertMatch({error, bad_size}, new([{"size", -10}])),
    ?assertMatch({error, bad_size}, new([{"size", 0}])).


%%-----------------------------------------------------------------------------
%% @doc Test deleting a new buffer.
%% @spec delete_test() -> ok
%% @end
-spec(delete_test() -> ok).
%%-----------------------------------------------------------------------------
delete_test() ->
    ?assertMatch(ok, delete(#buf{size=5, in=[1,2], out=[3]})).

-endif. % TEST

