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


%%====================================================================
%% External functions
%%====================================================================
test() ->
    try 
        debug_helper:start(),
        %debug_helper:trace(tuple_space),
        debug_helper:trace(ts_server),
	%application:load(tuple_space),
	%application:start(tuple_space),
        tuple_space:start(type, ["dets_storage_tuple"]),
        start_subscriber(),
        start_slave(5),
        start_master(20),
        sleep(3000),
        error_logger:info_msg("****Last Size of tuple space ~p~n", [tuple_space:size(node())])
    catch
        Ex ->
            io:format("test caught ~p~n", [Ex])
            %throw Ex
    end.


%%====================================================================
%% Internal functions
%%====================================================================

start_subscriber() ->
    spawn(fun() -> tuple_space:subscribe(node(), {tuple_record, '_'}),
	listen_events/0 end).


start_master(0) ->
    true;
start_master(N) ->
    error_logger:info_msg("Adding tuples ~p ~n", [N]),
    spawn(fun() -> tuple_space:put(node(), {tuple_record, N}) end),
    start_master(N-1).



start_slave(0) ->
    true;
start_slave(N) ->
    spawn(fun() -> slave_loop(N) end),
    start_slave(N-1).

slave_loop(N) ->
   Tuple = tuple_space:get(node(), {tuple_record, '_'}, 100),
   error_logger:info_msg("Getting tuples ~p for N ~p~n", [Tuple, N]),
   sleep(1000),
   slave_loop(N).
      

sleep(T) ->
    receive
        after T -> true
    end.
 
listen_events() ->
    receive 
        {tuple_added, Tuple} ->
             error_logger:info_msg("Notified tuples ~p ~n", [Tuple]),
	listen_events();
	Any ->
             error_logger:info_msg("Unknown event ~p ~n", [Any]),
	listen_events()
    end.
