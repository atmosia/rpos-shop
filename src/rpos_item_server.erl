-module(rpos_item_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([add_item/3, get_item/2, update_item/4, remove_item/3, list_items/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("types.hrl").

-record(state, {connection}).

start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

add_item(Pid, Item, Author) -> gen_server:call(Pid, {add, Item, Author}).

get_item(Pid, Name) -> gen_server:call(Pid, {get, Name}).

update_item(Pid, Name, Item, Author) ->
    gen_server:call(Pid, {update, Name, Item, Author}).

remove_item(Pid, Name, Author) ->
    gen_server:call(Pid, {remove, Name, Author}).

list_items(Pid) -> gen_server:call(Pid, list).

init([Config]) ->
    gen_server:cast(self(), {connect, Config}),
    {ok, #state{}}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extras) -> {ok, State}.

handle_call({add, Item, Author}, _From, State) ->
    {reply, rpos_item:add_item(State#state.connection, Item, Author), State};
handle_call({get, Item}, _From, State) ->
    {reply, rpos_item:add_item(State#state.connection, Item), State};
handle_call({update, Name, Item, Author}, _From, State) ->
    {reply,
     rpos_item:update_item(State#state.connection, Name, Item, Author),
     State};
handle_call({remove, Name, Author}, _From, State) ->
    {reply, rpos_item:remove_item(State#state.connection, Name, Author),
     State};
handle_call(list, _From, State) ->
    {reply, rpos_item:list_items(State#state.connection), State}.

handle_cast({connect, Config}, State) ->
    #{db := DBConfig} = Config,
    {ok, Connection} = epgsql:connect(DBConfig),
    {noreply, State#state{connection=Connection}}.

handle_info(_Message, State) -> {noreply, State}.
