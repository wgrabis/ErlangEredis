-module(users_stats).
-behaviour(gen_server).

-export([
  start_link/0,
  stop/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2
]).

-export([
  add/1,
  add_points/2,
  score/1,
  top/1
]).

start_link() ->
  gen_server:start_link({local, users_stats}, users_stats, [], [])
.

stop() ->
  gen_server:cast(users_stats, stop)
.

init([]) ->
  {ok, Conn} = eredis:start_link(),
  {ok, Conn}
.

terminate(Reason, _) ->
  Reason,
  ok
.

handle_call(Message, _From, Conn) ->
  case Message of
  {addUser, Name} ->
    {ok, Reply} = eredis:q(Conn, ["ZADD", "users", "0", Name]),
    {reply, Reply, Conn};
  {addPoints, Name, Points} ->
    {ok, Reply} = eredis:q(Conn, ["ZINCRBY", "users", Points, Name]),
    {reply, Reply, Conn};
  {score, Name} ->
    {ok, Reply} = eredis:q(Conn, ["ZSCORE", "users", Name]),
    {reply, Reply, Conn};
  {topN, N} ->
    {ok, Reply} = eredis:q(Conn, ["ZREVRANGE", "users", "0", N]),
    {reply, Reply, Conn}
  end
.

handle_cast(stop, Conn) ->
  {stop, normal, Conn}
.

add(Name) ->
  gen_server:call(users_stats, {addUser, Name})
.

add_points(Name, Points) ->
  gen_server:call(users_stats, {addPoints, Name, Points})
.

score(Name) ->
  gen_server:call(users_stats, {score, Name})
.

top(N) ->
  gen_server:call(users_stats, {topN, N})
.
