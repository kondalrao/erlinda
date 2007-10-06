%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  Unit Tests for tuple_util
%%  APIs:
%%  
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(tuple_util_test).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         test/0
	 ]).


%%====================================================================
%% External functions
%%====================================================================
test() ->
    test_match(),
    test_conversion().

test_match() ->
    assert_equals(true, tuple_util:matches_tuple({1, 2, 3, 4}, {1, 2, 3, 4})),
    assert_equals(true, tuple_util:matches_tuple({1, 2, 3, 4}, {1, 2, 3, '_'})),
    assert_equals(true, tuple_util:matches_tuple({1, '_', 3, 4}, {1, 2, 3, '_'})),
    assert_equals(false, tuple_util:matches_tuple({1, 2, 3, 4}, {1, 2, 3, 4, 5})).

test_conversion() ->
    {record4, 1, 2, 3, 4} = tuple_util:tuple_to_record("record", {1, 2, 3, 4}),
    {record5, 1, 2, 3, 4, 5} = tuple_util:tuple_to_record("record", {1, 2, 3, 4, 5}),
    {1, 2, 3, 4} = tuple_util:record_to_tuple({record4, 1, 2, 3, 4}),
    {1, 2, 3, 4, 5} = tuple_util:record_to_tuple({record5, 1, 2, 3, 4, 5}).

assert_equals(Expected, Actual) when is_atom(Expected) -> 
    assert_equals(atom_to_list(Expected), atom_to_list(Actual));
assert_equals(Expected, Actual) when is_tuple(Expected) -> 
    assert_equals(tuple_to_list(Expected), tuple_to_list(Actual));
assert_equals(Expected, Actual) when is_list(Expected) ->
    case Actual of 
        Expected ->
	    true;
	_ ->
	    throw("Expected " ++ Expected ++ ", but found " ++ Actual)
    end.
