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
    ets:new(list_to_atom(TupleSpaceName), [bag]).


%%--------------------------------------------------------------------
%% Function: get/3
%% Description: performs a blocking get and returns the tuple. The
%% tuple is removed from the tuple space.
%% Returns: {ok, Tuple} |
%%%         {error, nomatch}
%%--------------------------------------------------------------------
get(TupleSpace, TemplateTuple, Timeout) ->
    Record = tuple_util:tuple_to_record(TupleSpace, TemplateTuple),
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
    mnesia:transaction(
       fun () -> mnesia:write(Record) 
    end).


%%--------------------------------------------------------------------
%% Function: delete/1
%% Description: deletes tuple space.
%% Returns: 
%%--------------------------------------------------------------------
delete(TupleSpace) ->
    ets:delete(TupleSpace);

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

start_mnesia_if_not_running() ->
    case mnesia:system_info(is_running) of
        yes -> 
            mnesia:start()
zzzz
        no -> false
    end.

%{atomic, Usernames} = mnesia:transaction(fun () -> mnesia:all_keys(user) end),
%{ok, Usernames}.
%mnesia:abort({vhost_already_exists, VHostPath})
% mnesia:index_read(user_vhost, VHostPath, #user_vhost.virtual_host)
% rabbit_misc:execute_mnesia_transaction(
  fun () ->
          qlc:e(qlc:q([Q || Q = #amqqueue{pid = Pid}
                                <- mnesia:table(durable_queues),
                            node(Pid) == Node]))
  end,
  fun (Queues) ->
          Queues1 = lists:map(fun start_queue_process/1, Queues),
          rabbit_misc:execute_simple_mnesia_transaction(
            fun () ->
                    lists:foreach(fun recover_queue/1, Queues1),
                    ok
            end)
  end).
% ok = mnesia:write(durable_queues, Q, write),
% mnesia:wread({amqqueue, QueueName})
%     {atomic, ok} = mnesia:transaction
                     (fun () ->
                              mnesia:foldl(fun (Exchange, Acc) ->
                                                   ok = mnesia:write(Exchange),
                                                   Acc
                                           end, ok, durable_exchanges)
                      end),
% ok = mnesia:dirty_write({rabbit_config, Key, Value}).
% case mnesia:dirty_read(ReadSpec) of [] ->

%dirty_read_all(TableName) ->
    mnesia:dirty_select(TableName, [{'$1',[],['$1']}]).

status() ->
    [{nodes, mnesia:system_info(db_nodes)},
     {running_nodes, mnesia:system_info(running_db_nodes)}].
	

is_db_empty() ->
    lists:all(fun (Tab) -> mnesia:dirty_first(Tab) == '$end_of_table' end,
              table_names()).

% mnesia:stop()


ensure_mnesia_dir() ->
    MnesiaDir = mnesia:system_info(directory) ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
        ok -> ok
    end.


cluster_nodes_config_filename() ->
    mnesia:system_info(directory) ++ "/cluster_nodes.config".

%% Take a cluster node config and create the right kind of node - a
%% standalone disk node, or disk or ram node connected to the
%% specified cluster nodes.
init_db(ClusterNodes) ->
    WasDiskNode = mnesia:system_info(use_dir),
    IsDiskNode = ClusterNodes == [] orelse
        lists:member(node(), ClusterNodes),
    case mnesia:change_config(extra_db_nodes, ClusterNodes -- [node()]) of
        {ok, []} ->
            if WasDiskNode and IsDiskNode ->
                    ok;
               WasDiskNode ->
                    throw({error, {cannot_convert_disk_node_to_ram_node,
                                   ClusterNodes}});
               IsDiskNode ->
                    ok = create_schema();
               true ->
                    throw({error, {unable_to_contact_cluster_nodes,
                                   ClusterNodes}})
            end;
        {ok, [_|_]} ->
            ok = ensure_schema_integrity(),
            ok = wait_for_tables(),
            ok = create_local_table_copies(
                   case IsDiskNode of
                       true  -> disc;
                       false -> ram
                   end);
        {error, Reason} ->
            %% one reason we may end up here is if we try to join
            %% nodes together that are currently running standalone or
            %% are members of a different cluster
            throw({error, {unable_to_join_cluster,
                           ClusterNodes, Reason}})
    end.

create_schema() ->
    mnesia:stop(),
    rabbit_misc:ensure_ok(mnesia:create_schema([node()]),
                          cannot_create_schema),
    rabbit_misc:ensure_ok(mnesia:start(),
                          cannot_start_mnesia),
    lists:foreach(fun ({Tab, TabArgs}) ->
                          case mnesia:create_table(Tab, TabArgs) of
                              {atomic, ok} -> ok;
                              {aborted, Reason} ->
                                  throw({error, {table_creation_failed,
                                                 Tab, TabArgs, Reason}})
                          end
                  end,
                  table_definitions()),
    ok.



create_local_table_copy(Tab, Type) ->
    StorageType = mnesia:table_info(Tab, storage_type),
    {atomic, ok} =
        if
            StorageType == unknown ->
                mnesia:add_table_copy(Tab, node(), Type);
            StorageType /= Type ->
                mnesia:change_table_copy_type(Tab, node(), Type);
            true -> {atomic, ok}
        end,
    ok.

wait_for_tables() ->
    case mnesia:wait_for_tables(table_names(), 30000) of
        ok -> ok;
        {timeout, BadTabs} ->
            throw({error, {timeout_waiting_for_tables, BadTabs}});
        {error, Reason} ->
            throw({error, {failed_waiting_for_tables, Reason}})
    end.



