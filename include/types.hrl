-record(item, { name :: string(),
                cost :: non_neg_integer(),
                price :: non_neg_integer(),
                type :: string(),
                brand :: string(),
                description :: string()
              }).
-type item() :: #item{}.

-record(item_count, { name :: string(),
                      count :: integer()
                    }).
-type item_count() :: #item_count{}.
