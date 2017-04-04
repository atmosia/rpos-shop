-module(rpos_item_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([add_item/2, get_item/2, update_item/3, remove_item/2, list_items/1]).
-export([add_variation/3, get_variation/3, update_variation/4,
         remove_variation/3, list_variations/2]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {connection}).

%%====================================================================
%% Public API
%%====================================================================

start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

add_item(Pid, Item) -> gen_server:call(Pid, {add, Item}).

get_item(Pid, Name) -> gen_server:call(Pid, {get, Name}).

update_item(Pid, Name, Item) ->
    gen_server:call(Pid, {update, Name, Item}).

remove_item(Pid, Name) ->
    gen_server:call(Pid, {remove, Name}).

list_items(Pid) -> gen_server:call(Pid, list).

add_variation(Pid, ItemName, Variation) ->
    gen_server:call(Pid, {add_variation, ItemName, Variation}).

get_variation(Pid, ItemName, Name) ->
    gen_server:call(Pid, {get_variation, ItemName, Name}).

update_variation(Pid, ItemName, Name, Variation) ->
    gen_server:call(Pid, {update_variation, ItemName, Name, Variation}).

remove_variation(Pid, ItemName, Name) ->
    gen_server:call(Pid, {remove_variation, ItemName, Name}).

list_variations(Pid, ItemName) ->
    gen_server:call(Pid, {list_variations, ItemName}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Config]) ->
    gen_server:cast(self(), {connect, Config}),
    {ok, #state{}}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extras) -> {ok, State}.

handle_call({add, Item}, _From, State) ->
    {reply, rpos_item:add_item(State#state.connection, Item), State};
handle_call({get, Item}, _From, State) ->
    {reply, rpos_item:add_item(State#state.connection, Item), State};
handle_call({update, Name, Item}, _From, State) ->
    {reply,
     rpos_item:update_item(State#state.connection, Name, Item),
     State};
handle_call({remove, Name}, _From, State) ->
    {reply, rpos_item:remove_item(State#state.connection, Name),
     State};
handle_call(list, _From, State) ->
    {reply, rpos_item:list_items(State#state.connection), State};
handle_call({add_variation, ItemName, Variation}, _From, State) ->
    {reply,
     rpos_item:add_variation(State#state.connection, ItemName,
                             Variation),
     State};
handle_call({get_variation, ItemName, Name}, _From, State) ->
    {reply, rpos_item:get_variation(State#state.connection, ItemName, Name),
     State};
handle_call({update_variation, ItemName, Name, Variation}, _From, State) ->
    {reply,
     rpos_item:update_variation(State#state.connection, ItemName, Name,
                                Variation),
     State};
handle_call({remove_variation, ItemName, Name}, _From, State) ->
    {reply,
     rpos_item:remove_variation(State#state.connection, ItemName, Name),
     State};
handle_call({list_variations, ItemName}, _From, State) ->
    {reply, rpos_item:list_variations(State#state.connection, ItemName),
     State}.

handle_cast({connect, Config}, State) ->
    #{db := DBConfig} = Config,
    {ok, Connection} = epgsql:connect(DBConfig),
    {noreply, State#state{connection=Connection}}.

handle_info(_Message, State) -> {noreply, State}.
