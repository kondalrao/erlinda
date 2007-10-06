%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  Utility functions for tuples.
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(tuple_util).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

%%--------------------------------------------------------------------
%% External exports for APIs
%%--------------------------------------------------------------------
-export([
	 matches_tuple/2,
	 tuple_to_record/2,
	 record_to_tuple/1
	 ]).
%%====================================================================
%% Function: matches_tuple/2
%% Description: match two tuples where '_' is considered as wild card.
%% Returns: true | false
%%====================================================================
matches_tuple(Tuple1, Tuple2) when is_tuple(Tuple1) and is_tuple(Tuple2) ->
    matches_tuple(tuple_to_list(Tuple1), tuple_to_list(Tuple2));
matches_tuple(Tuple1, List2) when is_tuple(Tuple1) ->
    matches_tuple(tuple_to_list(Tuple1), List2);
matches_tuple(List1, Tuple2) when is_tuple(Tuple2) and is_tuple(Tuple2) ->
    matches_tuple(List1, tuple_to_list(Tuple2));
matches_tuple([H|T1], [H|T2]) ->
    matches_tuple(T1, T2);
matches_tuple(['_'|T1], [H2|T2]) ->
    matches_tuple(T1, T2);
matches_tuple([H1|T1], ['_'|T2]) ->
    matches_tuple(T1, T2);
matches_tuple([H1|_T1], [H2|_T2]) ->
    false;
matches_tuple([], [_H|_T]) ->
    false;
matches_tuple([_H|_T], []) ->
    false;
matches_tuple([], []) ->
    true.


%%====================================================================
%% Function: tuple_to_record/2
%% Description: convert a tuple into record, taking in base name and
%%   original tuple. The number of tuple elements will be appended to the 
%%   base name.
%% Note A recod is just a tuple where first element is record name, so when
%% using Mnesia encode tuple so that we can save them in Mnesia directly.
%% Note: for ETS the first element of tuple is key whereas in Mnesia
%% the first element is record name and second is key.
%% Returns: record
%%====================================================================
tuple_to_record(BaseName, Tuple) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    Name = list_to_atom(BaseName ++ integer_to_list(length(List))),
    list_to_tuple([Name|List]).

%%====================================================================
%% Function: record_to_tuple/2
%% Description: removes first element of tuple (record name) and returns
%%    as a tuple.
%% Returns: tuple
%%====================================================================
record_to_tuple(Tuple) when is_tuple(Tuple) ->
    [H|T] = tuple_to_list(Tuple),
    list_to_tuple(T).
