%%%-------------------------------------------------------------------
%% @doc rpos_shop public API
%% @end
%%%-------------------------------------------------------------------

-module(rpos_shop).

-behaviour(application).

-export([build_item/4, add_item/1, get_item/1, update_item/2, remove_item/1,
         list_items/0]).
-export([build_variation/2, add_variation/2, get_variation/2,
         update_variation/3, remove_variation/2, list_variations/1]).

%% Application callbacks
-export([start/2, stop/1]).

-include("types.hrl").

-define(CONFIG_PATH, "config.json").
-define(LISTENER, rpos_shop_listener).

%%====================================================================
%% API
%%====================================================================

build_item(Name, Type, Brand, Description) ->
    #{name => Name, type => Type, brand => Brand,
      description => Description}.

add_item(Item) ->
    rpos_item_server:add_item(item_pid(), Item).

get_item(Item) ->
    rpos_item_server:get_item(item_pid(), Item).

update_item(Name, Item) ->
    rpos_item_server:update_item(item_pid(), Name, Item).

remove_item(Name) ->
    rpos_item_server:remove_item(item_pid(), Name).

list_items() ->
    rpos_item_server:list_items(item_pid()).

build_variation(Name, Cost) -> #{name => Name, cost => Cost}.

add_variation(ItemName, Variation) ->
    rpos_item_server:add_variation(item_pid(), ItemName, Variation).

get_variation(ItemName, Name) ->
    rpos_item_server:get_variation(item_pid(), ItemName, Name).

update_variation(ItemName, Name, Variation) ->
    rpos_item_server:update_variation(item_pid(), ItemName, Name, Variation).

remove_variation(ItemName, Name) ->
    rpos_item_server:remove_variation(item_pid(), ItemName, Name).

list_variations(ItemName) ->
    rpos_item_server:list_variations(item_pid(), ItemName).

start(_StartType, _StartArgs) ->
    Config = read_config(?CONFIG_PATH),
    #{auth := AuthConfig} = Config,
    Dispatch = cowboy_router:compile([
        {'_', [{"/", status_handler, #{}},
               {"/product/:name", product_handler, #{auth => AuthConfig}},
               {"/products", product_list_handler, #{}}
              ]}
    ]),
    Port = application:get_env(rpos_shop, port, 8082),
    {ok, _} = cowboy:start_clear(?LISTENER, 100,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    rpos_shop_sup:start_link(Config).

stop(_State) -> cowboy:stop_listener(?LISTENER).

%%====================================================================
%% Internal functions
%%====================================================================

item_pid() ->
    [{_, Pid, _, _}|_] = supervisor:which_children(rpos_shop_sup),
    Pid.


read_config(Path) ->
    case file:read_file(Path) of
        {ok, Data}      ->
            {Config} = jiffy:decode(Data, []),
            {DBConfig} = proplists:get_value(<<"database">>, Config, []),
            {AuthConfig} = proplists:get_value(<<"auth">>, Config, []),
            #{db => lists:foldl(fun parse_db_config/2, [], DBConfig),
              auth => lists:foldl(fun parse_auth_config/2, #{}, AuthConfig)};
        {error, enoent} -> throw({error, no_config_file})
    end.

parse_db_config({<<"host">>, Host}, Acc) ->
    [{host, binary:bin_to_list(Host)}|Acc];
parse_db_config({<<"port">>, Port}, Acc) -> [{port, Port}|Acc];
parse_db_config({<<"user">>, User}, Acc) ->
    [{username, binary:bin_to_list(User)}|Acc];
parse_db_config({<<"pass">>, Pass}, Acc) ->
    [{password, binary:bin_to_list(Pass)}|Acc];
parse_db_config({<<"name">>, Name}, Acc) ->
    [{database, binary:bin_to_list(Name)}|Acc].

parse_auth_config({<<"host">>, Host}, Acc) -> Acc#{host => Host};
parse_auth_config({<<"port">>, Port}, Acc) -> Acc#{port => Port}.
