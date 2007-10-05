%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  This module defines tuple space using underlying dets
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(dets_ts).
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
%% Description: creates a new dets 
%% Returns: DETS instance
%%--------------------------------------------------------------------
new(TupleSpaceName) ->
    case dets:open_file(TupleSpaceName, [{type, bag}, {file, [TupleSpaceName]}]) of
        {ok, TupleSpaceName} ->
            TupleSpaceName;
        {error, Reason} ->
            io:format("cannot open file ~p due to ~p~n", [TupleSpaceName, Reason]),
            erlang:raise(dets_open_failed)
    end.


%%--------------------------------------------------------------------
%% Function: get/3
%% Description: performs a blocking get and returns the tuple. The
%% tuple is removed from the tuple space.
%% Returns: {ok, Tuple} |
%%%         {error, nomatch}
%%--------------------------------------------------------------------
get(TupleSpace, TemplateTuple, Timeout) ->
    %error_logger:info_msg("dets got ~p~n", [X]),
    case dets:match_object(TupleSpace, TemplateTuple, 1) of
        '$end_of_table' -> 
            {error, nomatch};
        {[], _Cont} -> 
            {error, nomatch};
        {[Match|_], _Cont} -> 
            dets:delete_object(TupleSpace, Match),
            {ok, Match}
    end.

%%--------------------------------------------------------------------
%% Function: size/1
%% Description: returns number of tuples
%% Returns: number of tuples
%%--------------------------------------------------------------------
size(TupleSpace) ->
    dets:info(TupleSpace, size).

%%--------------------------------------------------------------------
%% Function: put/2
%% Description: adds a tuple to the tuple space.
%% Returns: 
%%--------------------------------------------------------------------
put(TupleSpace, Tuple) ->
    dets:insert(TupleSpace, Tuple).


%%--------------------------------------------------------------------
%% Function: delete/1
%% Description: deletes tuple space.
%% Returns: 
%%--------------------------------------------------------------------
delete(TupleSpace) ->
    dets:close(TupleSpace).

