%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  This module defines tuple space using underlying mnesia
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(mnesia_ts).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

-record(tuple_space_entry, {tuple_key, tuple_space_name, tuple, duplicate_count}).

%%--------------------------------------------------------------------
%% External exports for APIs
%%--------------------------------------------------------------------
-export([
	 new/1,
	 get/3,
	 size/1,
	 put/2,
	 delete/1,
	 delete/2
	 ]).


%%--------------------------------------------------------------------
%% Function: new/1
%% Description: news a new mnesia table
%% Returns: tuple space name
%%--------------------------------------------------------------------
new(TupleSpace) when is_atom(TupleSpace) ->
    start_mnesia_if_not_running(),
    new_table(TupleSpace),
    TupleSpace;
new(TupleSpaceName) ->
    start_mnesia_if_not_running(),
    TupleSpaceName.


%%--------------------------------------------------------------------
%% Function: get/3
%% Description: performs a blocking get and returns the tuple. The
%% tuple is removed from the tuple space.
%% Returns: {ok, Tuple} |
%%%         {error, nomatch}
%%--------------------------------------------------------------------
get(BaseTupleSpace, TemplateTuple, Timeout) when is_list(BaseTupleSpace), is_tuple(TemplateTuple), is_number(Timeout) ->
    {TupleSpace, Record} = get_record(BaseTupleSpace, TemplateTuple),
    get(TupleSpace, Record, Timeout);
get(TupleSpace, Record, Timeout) when is_atom(TupleSpace), is_tuple(Record), is_number(Timeout) ->
    % mnesia:dirty_read(TemplateTuple), % mnesia:index_read(TemplateTuple, TemplateTuple, TemplateTuple) % mnesia:wread(TemplateTuple),
    %R = mnesia:dirty_match_object(Record),
    Result = mnesia:transaction(
          fun () -> mnesia:match_object(Record)                 %mnesia:select(TupleSpace, Record, 1, read)
       end),

    Reply = case Result of
               {atomic, []} -> {error, nomatch};
               {atomic, [R|_]} -> {ok, R};      %tuple_util:record_to_tuple(R)};
               {aborted, _} -> {error, nomatch}
        end,
    error_logger:info_msg("*** get parameters ~p -- ~p~n", [Record, Reply]),
    Reply.

%%--------------------------------------------------------------------
%% Function: size/1
%% Description: returns number of tuples
%% Returns: number of tuples
%%--------------------------------------------------------------------
size(TupleSpace) when is_atom(TupleSpace) ->
    mnesia:table_info(TupleSpace, size).

%%--------------------------------------------------------------------
%% Function: put/2
%% Description: adds a tuple to the tuple space.
%% Returns: 
%%--------------------------------------------------------------------
put(BaseTupleSpace, Tuple) when is_list(BaseTupleSpace), is_tuple(Tuple) ->
    {TupleSpace, Record} = get_record(BaseTupleSpace, Tuple),
    put(TupleSpace, Record);
put(TupleSpace, Record) when is_atom(TupleSpace), is_tuple(Record) ->
    %%%%%%%%ok = mnesia:dirty_write(Record),
    Response = mnesia:transaction(
       fun () -> mnesia:write(Record) 
    end),
    case Response of
        {atomic, ok} -> ok;
	{aborted, {no_exists, _}} ->
             error_logger:info_msg("*** Unexpectedly creating table ~p -- ~p ~n", [TupleSpace, Response]),
    	     new_table(TupleSpace),
    	     {atomic, ok} = mnesia:transaction(
       		fun () -> mnesia:write(Record) 
    	     end)
    end,
    error_logger:info_msg("*** after adding ~p size ~p~n", [Record, mnesia_ts:size(TupleSpace)]).


%%--------------------------------------------------------------------
%% Function: delete/1
%% Description: deletes tuple space.
%% Returns: 
%%--------------------------------------------------------------------
delete(TupleSpace) when is_atom(TupleSpace) ->
    catch mnesia:delete_table(TupleSpace).

%%--------------------------------------------------------------------
%% Function: delete/2
%% Description: deletes a tuple from tuple space.
%% Returns: 
%%--------------------------------------------------------------------
delete(BaseTupleSpace, Tuple) when is_list(BaseTupleSpace), is_tuple(Tuple) ->
    {TupleSpace, Record} = get_record(BaseTupleSpace, Tuple),
    delete(TupleSpace, Record);
delete(TupleSpace, Record) when is_atom(TupleSpace), is_tuple(Record) ->
    {atomic, ok} = mnesia:transaction(
       fun () -> mnesia:delete_object(Record) 
    end),
    error_logger:info_msg("*** deleted ~p -- ~p~n", [Record, mnesia_ts:size(TupleSpace)]).



%%--------------------------------------------------------------------
%% Private Functions
%%--------------------------------------------------------------------

start_mnesia_if_not_running() ->
    case mnesia:system_info(is_running) of
        no -> 
            mnesia:create_schema([node()]),
            mnesia:start(),
    	    error_logger:info_msg("********* starting mnesia.... ~n");
        yes -> 
    	    error_logger:info_msg("********* mnesia already running....~n"),
            false
    end.

new_table(TupleSpace) ->
    %mnesia:wait_for_tables(TupleSpace, 60000)
    {atomic, ok} = mnesia:create_table(TupleSpace,
                         [{type, bag}, {disc_copies, [node()]},
                          %{index, [word]},
                          {attributes, record_info(fields,tuple_space_entry)}
                        ]),
    error_logger:info_msg("*** created table ~p~n", [TupleSpace]).

get_all(TupleSpace) ->
    {atomic, Tuples} = mnesia:transaction(fun () -> mnesia:all_keys(TupleSpace) end),
    Tuples.

dirty_get_all(TupleSpace) ->
    mnesia:dirty_select(TupleSpace, [{'$1',[],['$1']}]).

status() ->
    [{nodes, mnesia:system_info(db_nodes)},
     {running_nodes, mnesia:system_info(running_db_nodes)}].

is_db_empty(TupleSpaces) ->
    lists:all(fun (Tab) -> mnesia:dirty_first(Tab) == '$end_of_table' end, TupleSpaces).

mnesia_dir() ->
   mnesia:system_info(directory) ++ "/".


get_record(BaseName, Tuple) ->
    RecordName = tuple_util:record_name(BaseName, Tuple),
      %try
      %    X = mnesia:table_info(RecordName, version),
      %catch   
      %    throw:Term -> new_table(RecordName);
      %    exit:Reason -> new_table(RecordName);
      %    error:Reason -> new_table(RecordName)
      %end,
    %new_table(RecordName),
    {RecordName, tuple_util:tuple_to_record(RecordName, Tuple)}.
    

stop() ->
    mnesia:stop().
