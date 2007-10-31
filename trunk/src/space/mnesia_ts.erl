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
%% Description: news a new mnesia table
%% Returns: tuple space name
%%--------------------------------------------------------------------
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
get(BaseTupleSpace, TemplateTuple, Timeout) ->
    {TupleSpace, Record} = get_record(BaseTupleSpace, TemplateTuple),
    %mnesia:abort({vhost_already_exists, VHostPath})
    % mnesia:dirty_read(TemplateTuple),
    % mnesia:index_read(TemplateTuple, TemplateTuple, TemplateTuple)
    % mnesia:wread(TemplateTuple),

    %mnesia:transaction(
    %   fun () ->
    %       case mnesia:match_object(Record) of
    %           [] -> {error, nomatch};
    %           [R] -> {ok, R}
    %       end
    %   end).
    error_logger:info_msg("----------------get parameters ~p ~n", [Record]),
    %X = mnesia:dirty_match_object(Record),
    X = mnesia:transaction(
          fun () -> mnesia:select(TupleSpace, Record, 1, read)
       end),
    error_logger:info_msg(">>>>>================get parameters ~p -- ~p~n", [Record, X]),
    X.

%%--------------------------------------------------------------------
%% Function: size/1
%% Description: returns number of tuples
%% Returns: number of tuples
%%--------------------------------------------------------------------
size(BaseTupleSpace) ->
    mnesia:table_info(BaseTupleSpace, size).

%%--------------------------------------------------------------------
%% Function: put/2
%% Description: adds a tuple to the tuple space.
%% Returns: 
%%--------------------------------------------------------------------
put(BaseTupleSpace, Tuple) ->
    {TupleSpace, Record} = get_record(BaseTupleSpace, Tuple),
    %ok = mnesia:dirty_write(Record),
    mnesia:transaction(
       fun () -> mnesia:write(Record) 
    end).


%%--------------------------------------------------------------------
%% Function: delete/1
%% Description: deletes tuple space.
%% Returns: 
%%--------------------------------------------------------------------
delete(BaseTupleSpace) ->
    catch mnesia:delete_table(BaseTupleSpace).

%%--------------------------------------------------------------------
%% Function: delete/2
%% Description: deletes a tuple from tuple space.
%% Returns: 
%%--------------------------------------------------------------------
delete(BaseTupleSpace, Tuple) ->
    {TupleSpace, Record} = get_record(BaseTupleSpace, Tuple),
    mnesia:transaction(
       fun () -> mnesia:delete(Record) 
    end).



%%--------------------------------------------------------------------
%% Private Functions
%%--------------------------------------------------------------------

start_mnesia_if_not_running() ->
    case mnesia:system_info(is_running) of
        no -> 
            mnesia:create_schema([node()]),
            mnesia:start();
        yes -> false
    end.
new_table(TupleSpace) ->
    %mnesia:wait_for_tables(TupleSpace, 60000)
    %delete(TupleSpace),
io:format("creating table ~p~n", [TupleSpace]),
    mnesia:create_table(TupleSpace,
                         [{type, duplicate_bag}, {ram_copies, [node()]}
                          %{index, [word]},
                          %{attributes, record_info(fields,TupleSpace)}
                        ]).

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
    new_table(RecordName),
    {RecordName, tuple_util:tuple_to_record(RecordName, Tuple)}.
