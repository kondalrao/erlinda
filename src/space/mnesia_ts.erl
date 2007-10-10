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
	 create/1,
	 get/3,
	 size/1,
	 put/2,
	 delete/1
	 ]).


%%--------------------------------------------------------------------
%% Function: create/1
%% Description: creates a new mnesia table
%% Returns: tuple space name
%%--------------------------------------------------------------------
create(TupleSpaceName) ->
    start_mnesia_if_not_running(),
    create_table(TupleSpaceName).


%%--------------------------------------------------------------------
%% Function: get/3
%% Description: performs a blocking get and returns the tuple. The
%% tuple is removed from the tuple space.
%% Returns: {ok, Tuple} |
%%%         {error, nomatch}
%%--------------------------------------------------------------------
get(TupleSpace, TemplateTuple, Timeout) ->
    Record = tuple_util:tuple_to_record(TupleSpace, TemplateTuple),
    %mnesia:abort({vhost_already_exists, VHostPath})
    % mnesia:dirty_read(TemplateTuple),
    % mnesia:index_read(TemplateTuple, TemplateTuple, TemplateTuple)
    % mnesia:wread(TemplateTuple),
    mnesia:transaction(
       fun () ->
           case mnesia:match_object(Record) of
               [] -> {error, nomatch};
               [R] -> {ok, R}
           end
       end).


%%--------------------------------------------------------------------
%% Function: size/1
%% Description: returns number of tuples
%% Returns: number of tuples
%%--------------------------------------------------------------------
size(TupleSpace) ->
    mnesia:table_info(TupleSpace, size).

%%--------------------------------------------------------------------
%% Function: put/2
%% Description: adds a tuple to the tuple space.
%% Returns: 
%%--------------------------------------------------------------------
put(TupleSpace, Tuple) ->
    Record = tuple_util:tuple_to_record(TupleSpace, Tuple),
    %ok = mnesia:dirty_write(Record),
    mnesia:transaction(
       fun () -> mnesia:write(Record) 
    end).


%%--------------------------------------------------------------------
%% Function: delete/1
%% Description: deletes tuple space.
%% Returns: 
%%--------------------------------------------------------------------
delete(TupleSpace) ->
    catch mnesia:delete_table(TupleSpace).

%%--------------------------------------------------------------------
%% Function: delete/2
%% Description: deletes a tuple from tuple space.
%% Returns: 
%%--------------------------------------------------------------------
delete(TupleSpace, Tuple) ->
    Record = tuple_util:tuple_to_record(TupleSpace, Tuple),
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
create_table(TupleSpace) ->
    %mnesia:wait_for_tables(TupleSpace, 60000)
    delete(TupleSpace),
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
