%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  Tuple Space uses Linda based memory model and provides following
%%  APIs:
%%  
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(tuple_space).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

-behaviour(application).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("tuple_space.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/2,
	 shutdown/0,
	 stop/1,
         test/0
	 ]).

%%--------------------------------------------------------------------
%% External exports for APIs
%%--------------------------------------------------------------------
-export([
	 put/2,
	 get/3,
	 size/1,
	 subscribe/2
	 ]).

test() ->
    %%dbg:tracer(),
    %%dbg:p(all,[c,sos,sol]),
    %%dbg:tpl(tuple_space, [{'_',[],[{message,{return_trace}}]}]),
    tuple_space:start(1, 2),
    error_logger:info_msg("Adding 3 tuples~n"),
    tuple_space:put(node(), {1, 2, 3}),
    tuple_space:put(node(), {1, 2, 3,4}),
    tuple_space:put(node(), {1, 2, 3,4}),
    tuple_space:put(node(), {1, 2, 3,4,5}),
    error_logger:info_msg("Size of tuple space ~p~n", [tuple_space:size(node())]),
    error_logger:info_msg("Getting and removing first tuple ~p~n", [tuple_space:get(node(), {'$1', '$2', '$3'},2)]),
    error_logger:info_msg("Getting and removing second tuple ~p~n", [tuple_space:get(node(), {'$1', '$2', '$3', '$4'},2)]),
    error_logger:info_msg("Getting and removing third tuple ~p~n", [tuple_space:get(node(), {'$1', '$2', '$3', '$4', '$5'},2)]),
    error_logger:info_msg("Getting and removing non existing tuple~p~n", [tuple_space:get(node(), {'$1', '$2', '$3', '$4', '$5'},2)]),
    error_logger:info_msg("Size of tuple space ~p~n", [tuple_space:size(node())]),

    error_logger:info_msg("Adding another tuples~n"),
    tuple_space:put(node(), {1, 2, 3, 4}),
    error_logger:info_msg("Size of tuple space again ~p~n", [tuple_space:size(node())]),
    error_logger:info_msg("Getting and removing second tuple ~p~n", [tuple_space:get(node(), {'$1', '$2', '$3', '$4'},2)]),
    error_logger:info_msg("Size of tuple space ~p~n", [tuple_space:size(node())]).

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
    case ts_sup:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Called to shudown the tuple_space application.
%% @spec shutdown() -> ok 
%% @end
%%--------------------------------------------------------------------
shutdown() ->
    application:stop(tuple_space).


%%--------------------------------------------------------------------
%% @doc Stores a tuple
%% Types:
%%  Node = node()
%%  Tuple = tuple()
%% </pre>
%% @spec put(Tuple) -> ok
%% @end
%%--------------------------------------------------------------------
put(Node, Tuple) ->
    ts_server:put(Node, Tuple).

%%--------------------------------------------------------------------
%% @doc Removes a matching tuple and returns it. If the no tuple
%%    matches, then it waits until tuple is found. If there are more than
%%    one tuples matches then a random tuple will be returned.
%% <pre>
%% Types:
%%  Node = node()
%%  TemplateTuple = tuple()
%%  Timeout = atom()
%% </pre>
%% @spec get(TemplateTuple, Timeout) -> {ok, Tuple} | {timeout, Reason} | {error, Reason} | EXIT
%% @end
%%--------------------------------------------------------------------
get(Node, TemplateTuple, Timeout) ->
    ts_server:get(Node, TemplateTuple, Timeout).


%%--------------------------------------------------------------------
%% @doc Returns number of tuples in the tuple space.
%% <pre>
%% Types:
%%  Node = node()
%% </pre>
%% @spec size() -> {ok, term} | {error, Reason} | EXIT
%% @end
%%--------------------------------------------------------------------
size(Node) -> 
    ts_server:size(Node).

%%--------------------------------------------------------------------
%% @doc Subscribes the caller to notifications of tuple change for
%% the matching template tuple. When a tuple is added or removed the caller 
%% of this function will be sent a message of the form {tuple_added, tuple}
%% or {tuple_removed, tuple}.
%% <pre>
%% Types:
%%  Node = node()
%%  TemplateTuple = tuple()
%% </pre>
%% @spec subscribe(Node, TemplateTuple) -> bool() | EXIT
%% @end
%%--------------------------------------------------------------------
subscribe(Node, TemplateTuple) ->
    ts_server:subscribe(Node, TemplateTuple).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Called upon the termintion of an application.
%%--------------------------------------------------------------------
stop(State) ->
    ok.

