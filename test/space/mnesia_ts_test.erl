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
-module(mnesia_ts_test).
-author("Shahzad Bhatti <bhatti@plexobject.com> [http://bhatti.plexobject.com]").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         test/0
	 ]).

-record(mydata, {key, value, other}).

%%====================================================================
%% External functions
%%====================================================================
test() ->
    %debug_helper:start(),
    %debug_helper:trace(ts_mnesia),
    mnesia_ts:new(mydata),
    mnesia_ts:get(mydata, #mydata{key = 5, value = 10, other = 15}, 2),
    mnesia_ts:put(mydata, #mydata{key = 1, value = 2, other = 3}),
    mnesia_ts:put(mydata, #mydata{key = 10, value = 20, other = 30}),
    %%%
    2 = mnesia_ts:size(mydata),
    %%%
    Template = #mydata{key = '_', value = 2, other = '_'},
    {ok, #mydata{key=1, value=2, other=3}} = mnesia_ts:get(mydata, Template, 2),
    %%%
    mnesia_ts:delete(mydata, #mydata{key = 1, value = 2, other = 3}),
    %%%
    1 = mnesia_ts:size(mydata).

test2() ->
    Base = "baseName",
    mnesia_ts:new(Base),
    mnesia_ts:get(Base, {5, 10, 15}, 2),
    mnesia_ts:put(Base, {1, 2, 3}),
    mnesia_ts:put(Base, {10, 20, 30}),
    %%%
    2 = mnesia_ts:size(basename3),
    %%%
    Template = {'_', 2, '_'},
    {ok, {1, 2, 3}} = mnesia_ts:get(Base, Template, 2),
    %%%
    mnesia_ts:delete(Base, {1, 2, 3}),
    %%%
    1 = mnesia_ts:size(basename3).

