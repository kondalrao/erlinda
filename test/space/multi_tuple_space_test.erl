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
-module(multi_tuple_space_test).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         test/0
	 ]).


%-record(base3, {one, two, three}).

%%====================================================================
%% External functions
%%====================================================================
test() ->
    try 
        %debug_helper:start(),
        %%%%%debug_helper:trace(multi_tuple_space),
        %debug_helper:trace(ts_server),
        %debug_helper:trace(ts_mnesia),
        %%%%%debug_helper:trace(ts_ets),
        %%%%%debug_helper:trace(multi_tuple_space_test),
	%%%%%application:load(multi_tuple_space),
	%%%%%application:start(multi_tuple_space),
        multi_tuple_space:start(type, ["dets_storage_tuple"]),
        {ok, Pid} = multi_tuple_space:start(type, ["dets_storage_tuple"]),
        start_slave(Pid, 5),
        start_subscriber(Pid),
        sleep(1000), % let it register
        start_master(Pid, 5),
        error_logger:info_msg("          ****Finished starting...~n"),
        sleep(5000),
        error_logger:info_msg("          ****Last Size of tuple space ~p~n", [multi_tuple_space:size(Pid)])
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
        multi_tuple_space:subscribe(Pid, {multi_tuple_record, '_'}),
	listen_events()
        end).


start_master(Pid, 0) ->
    true;
start_master(Pid, N) ->
    error_logger:info_msg("          Adding tuples ~p ~n", [N]),
    spawn(fun() -> multi_tuple_space:put(Pid, {multi_tuple_record, N}) end),
    start_master(Pid, N-1).



start_slave(Pid, 0) ->
    true;
start_slave(Pid, N) ->
    spawn(fun() -> slave_loop(Pid, N) end),
    start_slave(Pid, N-1).

slave_loop(Pid, N) ->
   Tuple = multi_tuple_space:get(Pid, {multi_tuple_record, '_'}, 10000),
   error_logger:info_msg("          Getting tuples ~p for N ~p~n", [Tuple, N]),
   sleep(1000),
   slave_loop(Pid, N).
      

sleep(T) ->
    receive
        after T -> true
    end.
 
listen_events() ->
    receive 
        {multi_tuple_added, Tuple} ->
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


