%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  This module defines tuple space using underlying ets
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(ets_ts).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

%%--------------------------------------------------------------------
%% External exports for APIs
%%--------------------------------------------------------------------
-export([
	 new/1,
	 get/3,
	 size/1,
	 put/2,
	 delete/1
	 ]).


%%--------------------------------------------------------------------
%% Function: new/1
%% Description: news a new ets 
%% Returns: ETS instance
%%--------------------------------------------------------------------
new (TupleSpaceName) ->
    ets:new(list_to_atom(TupleSpaceName), [bag]).


%%--------------------------------------------------------------------
%% Function: get/3
%% Description: performs a blocking get and returns the tuple. The
%% tuple is removed from the tuple space.
%% Returns: {ok, Tuple} |
%%%         {error, nomatch}
%%--------------------------------------------------------------------
get(TupleSpace, TemplateTuple, Timeout) ->
    case ets:match_object(TupleSpace, TemplateTuple, 1) of
        '$end_of_table' -> 
            {error, nomatch};
        {[Match], _Cont} -> 
            ets:delete_object(TupleSpace, Match),
            {ok, Match}
    end.

%%--------------------------------------------------------------------
%% Function: size/1
%% Description: returns number of tuples
%% Returns: number of tuples
%%--------------------------------------------------------------------
size(TupleSpace) ->
    ets:info(TupleSpace, size).

%%--------------------------------------------------------------------
%% Function: put/2
%% Description: adds a tuple to the tuple space.
%% Returns: 
%%--------------------------------------------------------------------
put(TupleSpace, Tuple) ->
    ets:insert(TupleSpace, Tuple).


%%--------------------------------------------------------------------
%% Function: delete/1
%% Description: deletes tuple space.
%% Returns: 
%%--------------------------------------------------------------------
delete(TupleSpace) ->
    ets:delete(TupleSpace).

