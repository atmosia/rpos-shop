-module(rpos_item).

-export([add_item/3, get_item/2, update_item/4, remove_item/3, list_items/1]).

-include("types.hrl").

add_item(Conn, Item, Author) ->
    Query = "INSERT INTO
                product(name, cost, price, type, brand, description,
                        created_by)
            VALUES ($1, $2, $3, $4, $5, $6, $7)",
    case epgsql:equery(Conn, Query, item_list(Item) ++ [Author]) of
        {error, {error, error, _Pid, foreign_key_violation, _Msg, _Opts}} ->
            {error, no_such_type};
        {ok, _N} -> ok
    end.

get_item(Conn, Name) ->
    Query = "SELECT * FROM product WHERE name=$1 AND deleted='f'",
    case epgsql:equery(Conn, Query, [Name]) of
        {ok, _Cols, []} -> {error, no_product};
        {ok, _Cols, [Row]} -> {ok, build_item(Row)}
    end.

update_item(Conn, Name, Item, Author) ->
    ok = remove_item(Conn, Name, Author),
    add_item(Conn, Item, Author).

remove_item(Conn, Name, Author) ->
    Query = "UPDATE product
             SET deleted='t', deleted_by=$2, deleted_on=NOW()
             WHERE name=$1 AND deleted='f'",
    case epgsql:equery(Conn, Query, [Name, Author]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, no_product}
    end.

list_items(Conn) ->
    Query = "SELECT * FROM product WHERE deleted='f'",
    case epgsql:equery(Conn, Query, []) of
        {ok, _Rows, Cols} -> {ok, lists:map(fun build_item/1, Cols)}
    end.

build_item({Name, Cost, Price, Type, Brand, Description, _Deleted,
            _CreatedBy, _DeletedBy, _CreatedOn, _DeletedOn}) ->
    #item{name=Name, cost=Cost, price=Price, type=Type, brand=Brand,
          description=Description}.

item_list(#item{name=Name, cost=Cost, price=Price, type=Type, brand=Brand,
                description=Description}) ->
    [Name, Cost, Price, Type, Brand, Description].
