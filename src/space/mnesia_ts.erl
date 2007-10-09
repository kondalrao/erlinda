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
%% Description: creates a new mnesia table
%% Returns: tuple space name
%%--------------------------------------------------------------------
new(TupleSpaceName) ->
    start_mnesia_if_not_running(),
    create_table_if_not_exist(TupleSpaceName).


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
    ets:info(TupleSpace, size).

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
    ets:delete(TupleSpace).

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
create_table_if_not_exist(Table) ->
    %mnesia:wait_for_tables(Table, 60000)
    mnesia:create_table(Table,
                         [{type, duplicate_bag}, {ram_copies, [node()]}
                          %{index, [word]},
                          %{attributes, record_info(fields,Table)}
                        ]).

get_all(Table) ->
    {atomic, Tuples} = mnesia:transaction(fun () -> mnesia:all_keys(Table) end),
    Tuples.

dirty_get_all(Table) ->
    mnesia:dirty_select(Table, [{'$1',[],['$1']}]).

status() ->
    [{nodes, mnesia:system_info(db_nodes)},
     {running_nodes, mnesia:system_info(running_db_nodes)}].

is_db_empty(Tables) ->
    lists:all(fun (Tab) -> mnesia:dirty_first(Tab) == '$end_of_table' end, Tables).

mnesia_dir() ->
   mnesia:system_info(directory) ++ "/".



