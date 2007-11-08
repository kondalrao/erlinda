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
-module(multi_ts_server).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

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
-record(state, {nodes=[]}).


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
start_link(StartArgs) ->
    %gen_server:start_link({local, ?SERVER}, ?MODULE, StartArgs, []).
    gen_server:start_link(?MODULE, [], []).

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
    error_logger:info_msg("multi_ts_server:init/1 starting ~n"),
    Nodes = [node()],
    {ok, #state{nodes = Nodes}}.

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
    Node = get_node(TemplateTuple, State),
    {reply, Node, State};

handle_call({size, {}}, From, State) ->
    {reply, 2, State};

handle_call({crash, {}}, From, State) ->
    1 = 2;

handle_call({subscribe, {TemplateTuple, Subscriber}}, From, State) ->
    Node = get_node(TemplateTuple, State),
    {reply, Node, State};

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
    Node = get_node(Tuple, State),
    {noreply, State};
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
    {noreply, State};
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    error_logger:info_msg("multi_ts_server:terminate/2 shutting down.~n", []),
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
get_node(Tuple, State) ->
    TupleList = tuple_to_list(Tuple), 
    Nodes = State#state.nodes,
    NodeIndex = erlang:hash(lists:nth(1, TupleList), length(TupleList)) rem length(Nodes),
    lists:nth(NodeIndex+1, Nodes).
