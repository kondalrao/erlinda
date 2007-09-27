%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  The server for Tuple Space 
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(ts_server).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("tuple_space.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 stop/0
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


%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% record definitions
%%--------------------------------------------------------------------
-record(state, {}).

%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc Stores a tuple
%% Types:
%%  Node = node()
%%  Tuple = tuple()
%% </pre>
%% @spec put(Tuple) -> ok
%% @end
%%--------------------------------------------------------------------
put(Node, Tuple) when is_tuple(Tuple) ->
    gen_server:cast({?SERVER, Node}, {put, Tuple}).

%%--------------------------------------------------------------------
%% @doc Removes a matching tuple and returns it. If the no tuple
%%    matches, then it waits until tuple is found. If there are more than
%%    one tuples matches then a random tuple will be returned.
%% <pre>
%% Types:
%%  Node = node()
%%  TemplateTuple = tuple()
%%  Timeout = integer()
%% </pre>
%% @spec get(TemplateTuple, Timeout) -> {ok, Tuple} | {timeout, Reason} | {error, Reason} | EXIT
%% @end
%%--------------------------------------------------------------------
get(Node, TemplateTuple, Timeout) when is_tuple(TemplateTuple), is_integer(Timeout) ->
    gen_server:call({?SERVER, Node}, {get, {TemplateTuple, Timeout}}).

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
    gen_server:call({?SERVER, Node}, {size, {}}).

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
    gen_server:call({?SERVER, Node}, {subscribe, {TemplateTuple, self()}}).


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    error_logger:info_msg("ts_server:init/1 starting~n", []),
    process_flag(trap_exit, true),
    {ok, {ets:new(tuple_space, [duplicate_bag]), []}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({get, {TemplateTuple, Timeout}}, From, {TupleSpace, Subscriptions} = State) ->
io:format("~n~n~n====finding match for ~p in ~p~n~n~n", [TemplateTuple, Timeout]),
    JJ = ets:match_object(TupleSpace, TemplateTuple), 
io:format("~n~n~n====found ~p~n~n~n", [JJ]),
    case ets:match_object(TupleSpace, TemplateTuple) of
        {[], _Cont} -> 
            {reply, {error, no_match}, State};
        '$end_of_table' -> 
            {reply, {error, no_match}, State};
        {[Match], _Cont} -> 
            TupleSpace1 = ets:delete_object(Match),
            NewState = {TupleSpace1, Subscriptions},
            {reply, {ok, Match}, NewState}
    end;
handle_call({size, {}}, From, {TupleSpace, Subscriptions} = State) ->
    Size = ets:info(TupleSpace, size),
    {reply, {ok, Size}, State};
handle_call({subscribe, {TemplateTuple, Subscriber}=Subscription}, From, {TupleSpace, Subscriptions} = State) ->
    NewSubscriptions = [Subscription|lists:delete(Subscription, Subscriptions)],
    NewState         = {TupleSpace, NewSubscriptions},
    {reply, true, NewState};
handle_call(Request, From, {TupleSpace, Subscriptions} = State) ->
    Reply = ok,
    {reply, Reply, State}.




%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};


handle_cast({put, Tuple}, {TupleSpace, Subscriptions} = State) -> % Remember that State is a ets()
io:format("~n~n~n====inserting ~p in ~p~n~n~n", [Tuple, Subscriptions]),

    TupleSpace1 = ets:insert(TupleSpace, Tuple),
    lists:foldl(fun({TemplateTuple, Subscriber}, Tuple) -> 
        case matches_tuple(TemplateTuple, Tuple) of
            true ->
                Subscriber ! {notify_tuple_added, Tuple}
        end
    end, Tuple, Subscriptions),
    NewState = {TupleSpace1, Subscriptions},
    {noreply, NewState};
handle_cast(Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
matches_tuple(Tuple1, Tuple2) when is_tuple(Tuple1) and is_tuple(Tuple2) ->
    matches_tuple(tuple_to_list(Tuple1), tuple_to_list(Tuple2));
matches_tuple(Tuple1, List2) when is_tuple(Tuple1) ->
    matches_tuple(tuple_to_list(Tuple1), List2);
matches_tuple(List1, Tuple2) when is_tuple(Tuple2) and is_tuple(Tuple2) ->
    matches_tuple(List1, tuple_to_list(Tuple2));
matches_tuple([H|T1], [H|T2]) ->
    matches_tuple(T1, T2);
matches_tuple(['_'|T1], [H2|T2]) ->
    matches_tuple(T1, T2);
matches_tuple([H1|T1], ['_'|T2]) ->
    matches_tuple(T1, T2);
matches_tuple([H1|_T1], [H2|_T2]) ->
    false;
matches_tuple([], [_H|_T]) ->
    false;
matches_tuple([_H|_T], []) ->
    false;
matches_tuple([], []) ->
    true.

