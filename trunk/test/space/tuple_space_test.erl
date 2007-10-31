%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  Unit Tests for Tuple Space 
%%  APIs:
%%  
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(tuple_space_test).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         test/0
	 ]).


-record(base3, {one, two, three}).

%%====================================================================
%% External functions
%%====================================================================
test() ->
    Record = #base3{one = 1, two = 2, three = 3},
    {TupleSpace, XRecord} = get_record("base", {1, 2, 3}),
    io:format("Created table ~p~n", [TupleSpace]),

    mnesia:transaction(
       fun () -> mnesia:write(Record) 
    end),

    Size = mnesia:table_info(base3, size),
    io:format("~p --- Size after adding ~p is ~p~n", [TupleSpace, Record, Size]),

    %%%Template = {base3, '_', 2, '_'},
    Template = #base3{one = '_', two = 2, three = '_'},
    Found = mnesia:transaction(
          fun () -> mnesia:match_object(TupleSpace, Template)
          %fun () -> mnesia:select(TupleSpace, Template, 1, read)
       end),
    io:format("~p --- Found ~p~n", [Found]),

    mnesia:transaction(
       fun () -> mnesia:delete(Found) 
    end),

    Size1 = mnesia:table_info(TupleSpace, size),
    io:format("Size after deleting ~p~n", [Size1]),

    mnesia:delete_table(TupleSpace),

    io:format("Deleted tuple space ~p~n", [TupleSpace]),

    mnesia:stop(),

    io:format("Stopped!!!!~n").


test1() ->
    try 
        debug_helper:start(),
        %debug_helper:trace(tuple_space),
        debug_helper:trace(ts_server),
        debug_helper:trace(ts_mnesia),
        %debug_helper:trace(ts_ets),
        %debug_helper:trace(tuple_space_test),
	%application:load(tuple_space),
	%application:start(tuple_space),
        tuple_space:start(type, ["dets_storage_tuple"]),
        {ok, Pid} = tuple_space:start(type, ["dets_storage_tuple"]),
        start_slave(Pid, 5),
        start_subscriber(Pid),
        sleep(1000), % let it register
        start_master(Pid, 5),
        error_logger:info_msg("          ****Finished starting...~n"),
        sleep(5000),
        error_logger:info_msg("          ****Last Size of tuple space ~p~n", [tuple_space:size(Pid)])
    catch
        Ex ->
            io:format("test caught ~p~n", [Ex])
            %throw Ex
    end.


%%====================================================================
%% Internal functions
%%====================================================================

start_subscriber(Pid) ->
    spawn(fun() -> 
        tuple_space:subscribe(Pid, {tuple_record, '_'}),
	listen_events()
        end).


start_master(Pid, 0) ->
    true;
start_master(Pid, N) ->
    error_logger:info_msg("          Adding tuples ~p ~n", [N]),
    spawn(fun() -> tuple_space:put(Pid, {tuple_record, N}) end),
    start_master(Pid, N-1).



start_slave(Pid, 0) ->
    true;
start_slave(Pid, N) ->
    spawn(fun() -> slave_loop(Pid, N) end),
    start_slave(Pid, N-1).

slave_loop(Pid, N) ->
   Tuple = tuple_space:get(Pid, {tuple_record, '_'}, 10000),
   error_logger:info_msg("          Getting tuples ~p for N ~p~n", [Tuple, N]),
   sleep(1000),
   slave_loop(Pid, N).
      

sleep(T) ->
    receive
        after T -> true
    end.
 
listen_events() ->
    receive 
        {tuple_added, Tuple} ->
             error_logger:info_msg("          listen_events Notified tuples ~p ~n", [Tuple]),
	     listen_events();
        {server_terminated} ->
             error_logger:info_msg("          listen_events server died, terminating~n"),
             exit(cancel_timer);
	Any ->
             error_logger:info_msg("          listen_events Unknown event ~p ~n", [Any]),
	listen_events()
    after 300000 -> 
        error_logger:info_msg("          listen_events didn't get any tuples in 5 minutes, trying again...~n")
    end.



get_record(BaseName, Tuple) ->
    RecordName = tuple_util:record_name(BaseName, Tuple),
    new_table(RecordName),
    {RecordName, tuple_util:tuple_to_record(RecordName, Tuple)}.


new_table(TupleSpace) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    io:format("creating table ~p~n", [TupleSpace]),
    %mnesia:create_table(TupleSpace,
    %                     [{type, duplicate_bag}, {disc_copies, [node()]}
    %                      %{attributes, record_info(fields,TupleSpace)}
    %                    ]).

    mnesia:create_table(base3,
                         [{type, ordered_set}, {ram_copies, [node()]},
                          %{index, [word]},
                          {attributes, record_info(fields,base3)}
                        ]).

