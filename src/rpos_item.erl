-module(rpos_item).

%% item settings
-export([add_item/2, get_item/2, update_item/3, remove_item/2, list_items/1]).
%% item variation settings
-export([add_variation/3, get_variation/3, update_variation/4,
         remove_variation/3, list_variations/2]).

%%--------------------------------------------------------------------
%% item methods
%%--------------------------------------------------------------------

add_item(Conn, Item) ->
    Query = "INSERT INTO
                product(name, cost, price, type, brand, description)
            VALUES ($1, $2, $3, $4)",
    case epgsql:equery(Conn, Query, item_list(Item)) of
        {error, {error, error, _Pid, foreign_key_violation, _Msg, _Opts}} ->
            {error, no_such_type};
        {ok, _N} -> ok
    end.

get_item(Conn, Name) ->
    Query = "SELECT * FROM product WHERE name=$1",
    case epgsql:equery(Conn, Query, [Name]) of
        {ok, _Cols, []} -> {error, no_product};
        {ok, _Cols, [Row]} -> {ok, build_item(Row)}
    end.

update_item(Conn, Name, Item) ->
    ok = remove_item(Conn, Name),
    add_item(Conn, Item).

remove_item(Conn, Name) ->
    Query = "DELETE FROM product WHERE name=$1",
    case epgsql:equery(Conn, Query, [Name]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, no_product}
    end.

list_items(Conn) ->
    Query = "SELECT * FROM product",
    case epgsql:equery(Conn, Query, []) of
        {ok, _Rows, Cols} -> {ok, lists:map(fun build_item/1, Cols)}
    end.

%%--------------------------------------------------------------------
%% variation methods
%%--------------------------------------------------------------------

add_variation(Conn, ItemName, Variation) ->
    Query = "INSERT INTO product_variation VALUES ($1, $2, $3)",
    case epgsql:equery(Conn, Query, [ItemName|variation_list(Variation)]) of
        {error, {error, error, _Pid, foreign_key_violation, _Msg, _Opts}} ->
            {error, no_such_product};
        {ok, _N} -> ok
    end.

get_variation(Conn, ItemName, Name) ->
    Query = "SELECT * FROM product_variation
             WHERE product_name=$1 AND variation = $2",
    case epgsql:equery(Conn, Query, [ItemName, Name]) of
        {ok, _Cols, []} -> {error, no_variation};
        {ok, _Cols, [Row]} -> {ok, build_variation(Row)}
    end.

update_variation(Conn, ItemName, Name, Variation) ->
    ok = remove_variation(Conn, ItemName, Name),
    add_variation(Conn, ItemName, Variation).

remove_variation(Conn, ItemName, Name) ->
    Query = "DELETE FROM product_variation
             WHERE product_name=$1 AND variation=$2",
    case epgsql:equery(Conn, Query, [ItemName, Name]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, no_variation}
    end.

list_variations(Conn, ItemName) ->
    Query = "SELECT * FROM product_variation WHERE product_name=$1",
    case epgsql:equery(Conn, Query, [ItemName]) of
        {ok, _Rows, Cols} -> {ok, lists:map(fun build_variation/1, Cols)}
    end.


%%====================================================================
%% Internal functions
%%====================================================================

build_item({Name, Type, Brand, Description}) ->
    #{name => Name, type => Type, brand => Brand,
      description => Description}.

item_list(#{name := Name, type := Type, brand := Brand,
            description := Description}) ->
    [Name, Type, Brand, Description].

build_variation({Product, Variation, Cost}) ->
    #{product => Product, variation => Variation, cost => Cost}.

variation_list(#{variation := Variation, cost := Cost}) ->
    [Variation, Cost].
