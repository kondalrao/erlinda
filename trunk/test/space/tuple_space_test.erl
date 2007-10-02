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
    %dbg:tracer(),
    %dbg:p(all,[c,sos,sol]),
    %dbg:tpl(tuple_space, [{'_',[],[{message,{return_trace}}]}]),
    tuple_space:start(type, ["dets_storage_tuple"]),
    start_master(20),
    start_slave(5),
    error_logger:info_msg("Size of tuple space ~p~n", [tuple_space:size(node())]),
    sleep(10000),
    error_logger:info_msg("Size of tuple space ~p~n", [tuple_space:size(node())]).


%%====================================================================
%% Internal functions
%%====================================================================


start_master(0) ->
    true;
start_master(N) ->
    error_logger:info_msg("Adding tuples ~p ~n", [N]),
    spawn(?MODULE, fun() -> tuple_space:put(node(), {tuple_record, N}) end),
    start_master(N-1).



start_slave(0) ->
    true;
start_slave(N) ->
    spawn(?MODULE, slave_loop/1, N),
    start_slave(N-1).

slave_loop(N) ->
   Tuple = tule_space:get({tuple_record, '_'}),
   error_logger:info_msg("Getting tuples ~p for N ~p~n", [Tuple, N]),
   sleep(1000),
   slave_loop(N).
      

sleep(T) ->
    receive
        after T -> true
    end.
 
