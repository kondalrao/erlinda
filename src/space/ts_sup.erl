%%%-------------------------------------------------------------------
%% @author Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]
%% @doc erlinda: The Erlang framework for writing parallel applications.
%%
%% == Contents ==
%%
%% {@section Introduction}<br/>
%% == Introduction ==
%%  The supervisor for Tuple Space 
%%
%% @copyright Shahzad Bhatti 2007
%% For license information see LICENSE.txt
%% 
%% @end
%%%-------------------------------------------------------------------
-module(ts_sup).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @spec start_link(StartArgs) -> {ok, pid()} | Error
%% @end
%%--------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy    = one_for_one,
    MaxRestarts        = 1000,
    MaxTimeBetRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
    
    ChildSpecs =
	[
	 {ts_server,
	  {ts_server, start_link, []},
	  permanent,
	  1000,
	  worker,
	  [ts_server]}
	 ],
    {ok,{SupFlags, ChildSpecs}}.
%%====================================================================
%% Internal functions
%%====================================================================
