-module(users_stats).

-export([new/0, add/2, add_points/3, get_top_n/2]).

new() ->
  eredis:start_link().

add(Conn, Username) ->
  eredis:q(Conn, ["ZADD", "users", "0", Username]).

add_points(Conn, Username, Points) ->
  eredis:q(Conn, ["ZINCRBY", "users", Points, Username]).

get_top_n(Conn, N) ->
  eredis:q(Conn, ["ZRANGE", "users", "0", N]).
