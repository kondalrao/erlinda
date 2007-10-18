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
	 start_link/1,
	 stop/0
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
%% gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% record definitions
%%--------------------------------------------------------------------
-record(state, {tuple_space, subscriptions=dict:new(), timers=[]}).


%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(TUPLE_SPACE_PROVIDER, ets_ts).   %% dets_ts, mnesia_ts

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link(StartArgs) ->
    %gen_server:start_link({local, ?SERVER}, ?MODULE, StartArgs, []).
    gen_server:start_link(?MODULE, StartArgs, []).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).     % named stateless server
stop(Pid) ->
    gen_server:cast(Pid, stop).         % stateful server

%%--------------------------------------------------------------------
%% @doc Stores a tuple
%% Types:
%%  Node = node()
%%  Tuple = tuple()
%% </pre>
%% @spec put(Tuple) -> ok
%% @end
%%--------------------------------------------------------------------
%put(Node, Tuple) when is_tuple(Tuple) ->
%    gen_server:cast({?SERVER, Node}, {put, Tuple}).
put(Pid, Tuple) when is_tuple(Tuple) ->
    gen_server:cast(Pid, {put, Tuple}).

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
%% @spec get(TemplateTuple, Timeout) -> {ok, Tuple} | {error, timeout} | {error, Reason} | EXIT
%% @end
%%--------------------------------------------------------------------
%get(Node, TemplateTuple, Timeout) when is_tuple(TemplateTuple), is_integer(Timeout) ->
%    gen_server:call({?SERVER, Node}, {get, {TemplateTuple, Timeout}}).
get(Pid, TemplateTuple, Timeout) when is_tuple(TemplateTuple), is_integer(Timeout) ->
    gen_server:call(Pid, {get, {TemplateTuple, Timeout}}).

%%--------------------------------------------------------------------
%% @doc Returns number of tuples in the tuple space.
%% <pre>
%% Types:
%%  Node = node()
%% </pre>
%% @spec size() -> {ok, term} | {error, Reason} | EXIT
%% @end
%%--------------------------------------------------------------------
%size(Node) -> 
%    gen_server:call({?SERVER, Node}, {size, {}}).
size(Pid) -> 
    gen_server:call(Pid, {size, {}}).

crash(Pid) -> 
    gen_server:call(Pid, {crash, {}}).

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
%subscribe(Node, TemplateTuple) ->
%    gen_server:call({?SERVER, Node}, {subscribe, {TemplateTuple, self()}}).
subscribe(Pid, TemplateTuple) ->
    gen_server:call(Pid, {subscribe, {TemplateTuple, self()}}).


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
    process_flag(trap_exit, true),
    error_logger:info_msg("ts_server:init/1 starting ~n"),
    TupleSpace = ?TUPLE_SPACE_PROVIDER:new("TupleSpaceName"),
    {ok, #state{tuple_space = TupleSpace}}.

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
handle_call({get, {TemplateTuple, Timeout}}, From, State) ->
    TupleSpace = State#state.tuple_space,
    Resp = ?TUPLE_SPACE_PROVIDER:get(TupleSpace, TemplateTuple, Timeout),
    case Resp of 
        {error, nomatch} ->
            NewSubscriptions = dict:store(TemplateTuple, {From, true}, State#state.subscriptions),
	    NewTimer = start_timeout_timer(From, TemplateTuple, Timeout),
            NewState = State#state{subscriptions = NewSubscriptions, timers = [NewTimer|State#state.timers]},
            {noreply, NewState};
        {ok, _} ->
            {reply, Resp, State}
    end;

handle_call({size, {}}, From, State) ->
    TupleSpace = State#state.tuple_space,
    Size = ?TUPLE_SPACE_PROVIDER:size(TupleSpace),
    {reply, {ok, Size}, State};

handle_call({crash, {}}, From, State) ->
    1 = 2;

handle_call({subscribe, {TemplateTuple, Subscriber}}, From, State) ->
    TupleSpace = State#state.tuple_space,
    NewSubscriptions = dict:store(TemplateTuple, {Subscriber, false}, State#state.subscriptions),
    NewState = State#state{subscriptions = NewSubscriptions},
    {reply, true, NewState};

handle_call(Request, From, State) ->
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

handle_cast({put, Tuple}, State) -> 
    TupleSpace = State#state.tuple_space,
    ?TUPLE_SPACE_PROVIDER:put(TupleSpace, Tuple),
    {Tuple, NewSubscriptions} = dict:fold(fun notify_tuple_added/3, {Tuple, State#state.subscriptions}, State#state.subscriptions),
    NewState = State#state{subscriptions = NewSubscriptions},
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
handle_info({'EXIT', Pid, Reason}, State) ->
    cleanup(State),
    %unlink(Res),
    %exit(Res, Reason),
    {noreply, State};
handle_info({timedout, From, TemplateTuple}, State) ->
    gen_server:reply(From, {timedout, TemplateTuple}),
    NewSubscriptions = dict:erase(TemplateTuple, State#state.subscriptions),
    NewState = State#state{subscriptions = NewSubscriptions},
    {noreply, NewState};
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    error_logger:info_msg("ts_server:terminate/2 shutting down.~n", []),
    cleanup(State),
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

%%
%%% notifies subscribers, where permanent subscribers are the processes
%%% that explicitly subscribed and temporary subscribers are the ones
%%% that just issue blocking get or read and wait for response and 
%%% their subscriptions is removed after tuple matches.
%%
notify_tuple_added(TemplateTuple, {From, TemporarySubscriber}, {Tuple, Subscriptions}) when TemporarySubscriber ->
    NewSubscriptions = case tuple_util:matches_tuple(TemplateTuple, Tuple) of
        true ->
            gen_server:reply(From, {ok, Tuple}),
            error_logger:info_msg("notify_tuple_added matched tuple ~p for ~p, will delete now~n", [Tuple, From]),
            dict:erase(TemplateTuple, Subscriptions);
        false ->
            Subscriptions
    end,
    {Tuple, NewSubscriptions};
notify_tuple_added(TemplateTuple, {From, TemporarySubscriber}, {Tuple, Subscriptions}) ->
    case tuple_util:matches_tuple(TemplateTuple, Tuple) of
        true ->
            error_logger:info_msg("notify_tuple_added matched tuple ~p for ~p~n", [Tuple, From]),
            From ! {tuple_added, Tuple};
        false ->
            false
    end,
    {Tuple, Subscriptions}.

%%
%%% this method notifies subscribers that the server is terminated.
%%
notify_server_terminated(_, {From, _TempSubscription}, _) ->
    error_logger:info_msg("notifying subscriber that server is dead ~p~n", [From]),
    catch From ! {server_terminated}.


%%
%%% this starts a process that waits and if the matching tuple is not found
%%% we send back timeout error
%%
start_timeout_timer(From, TemplateTuple, Timeout) when Timeout > 0 ->
    %%%% spawn_link is creating dump
    spawn(fun() -> timeout_loop(From, TemplateTuple, Timeout) end);
start_timeout_timer(From, TemplateTuple, Timeout) ->
    true.
timeout_loop(From, TemplateTuple, Timeout) ->
    receive 
        {cancel_timer} ->
            exit(cancel_timer);
	Any ->
             error_logger:info_msg("timeout_loop unknown event ~p ~n", [Any]),
	     timeout_loop(From, TemplateTuple, Timeout)
    after Timeout -> 
        ?SERVER ! {timedout, From, TemplateTuple}
    end.
    
%%
%%% this method terminates any timers that are running.
%%% we send back timeout error
%%
terminate_timers(Timer) ->
    %error_logger:info_msg("cancelling timer ~p~n", [Timer]),
    catch Timer ! {cancel_timer}.


%%
%%% this method cleans up resources when the server dies
%%
cleanup(State) ->
    TupleSpace = State#state.tuple_space,
    catch ?TUPLE_SPACE_PROVIDER:delete(TupleSpace),
    %%%
    catch dict:fold(fun notify_server_terminated/3, nil, State#state.subscriptions),
    catch lists:foreach(fun terminate_timers/1, State#state.timers).
