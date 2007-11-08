%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  Multi Tuple Space spawns multiple instances of tuple spaces on one or
%%  more nodes and it load balances the tuples across them using a simple
%%  hash based distribution scheme. Note that the hash requires that
%%  the first parameter of a tuple be always present even when looking up
%%  by template tuple. 
%%  
%%  APIs:
%%  
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(multi_tuple_space).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

-behaviour(application).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%-include("multi_tuple_space.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/2,
	 shutdown/0,
	 stop/1
	 ]).

%%--------------------------------------------------------------------
%% External exports for APIs
%%--------------------------------------------------------------------
-export([
	 put/2,
	 get/3,
	 size/1,
	 crash/1,
	 subscribe/2
	 ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc The starting point for an erlang application.
%% @spec start(Type, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start(Type, StartArgs) ->
    case ts_server:start_link(StartArgs) of
    %case ts_sup:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Called to shudown the multi_tuple_space application.
%% @spec shutdown() -> ok 
%% @end
%%--------------------------------------------------------------------
shutdown() ->
    application:stop(multi_tuple_space).


%%--------------------------------------------------------------------
%% @doc Stores a tuple
%% Types:
%%  Pid = pid()
%%  Node = node()
%%  Tuple = tuple()
%% </pre>
%% @spec put(Tuple) -> ok
%% @end
%%--------------------------------------------------------------------
put(Pid, Tuple) ->
    ts_server:put(Pid, Tuple).

%%--------------------------------------------------------------------
%% @doc Removes a matching tuple and returns it. If the no tuple
%%    matches, then it waits until tuple is found. If there are more than
%%    one tuples matches then a random tuple will be returned.
%% <pre>
%% Types:
%%  Pid = pid()
%%  Node = node()
%%  TemplateTuple = tuple()
%%  Timeout = atom()
%% </pre>
%% @spec get(TemplateTuple, Timeout) -> {ok, Tuple} | {timeout, Reason} | {error, Reason} | EXIT
%% @end
%%--------------------------------------------------------------------
get(Pid, TemplateTuple, Timeout) ->
    ts_server:get(Pid, TemplateTuple, Timeout).


%%--------------------------------------------------------------------
%% @doc Returns number of tuples in the tuple space.
%% <pre>
%% Types:
%%  Pid = pid()
%%  Node = node()
%% </pre>
%% @spec size() -> {ok, term} | {error, Reason} | EXIT
%% @end
%%--------------------------------------------------------------------
size(Pid) -> 
    ts_server:size(Pid).

crash(Pid) -> 
    ts_server:crash(Pid).

%%--------------------------------------------------------------------
%% @doc Subscribes the caller to notifications of tuple change for
%% the matching template tuple. When a tuple is added or removed the caller 
%% of this function will be sent a message of the form {tuple_added, tuple}
%% or {tuple_removed, tuple}.
%% <pre>
%% Types:
%%  Pid = pid()
%%  Node = node()
%%  TemplateTuple = tuple()
%% </pre>
%% @spec subscribe(Node, TemplateTuple) -> bool() | EXIT
%% @end
%%--------------------------------------------------------------------
subscribe(Pid, TemplateTuple) ->
    ts_server:subscribe(Pid, TemplateTuple).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Called upon the termintion of an application.
%%--------------------------------------------------------------------
stop(State) ->
    ok.

