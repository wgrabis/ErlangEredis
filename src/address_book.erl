-module(address_book).
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
  add_user/1,
  add_email/2,
  add_phone/2,
  remove_email/2,
  remove_phone/2,
  get_users/0,
  get_emails/1,
  get_phones/1
]).

start_link() ->
  gen_server:start_link({local, address_book}, address_book, [], [])
.

stop() ->
  gen_server:cast(address_book, stop)
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
    {ok, _Reply} = eredis:q(Conn, ["SADD", "book", Name]),
    {reply, ok, Conn};
  {addEmail, Name, Email} ->
    Key = string:concat(string:concat("book:", Name), ":emails"),
    {ok, _Reply} = eredis:q(Conn, ["SADD", Key, Email]),
    {reply, ok, Conn};
  {addPhone, Name, Phone} ->
    Key = string:concat(string:concat("book:", Name), ":phones"),
    {ok, _Reply} = eredis:q(Conn, ["SADD", Key, Phone]),
    {reply, ok, Conn};
  {removeEmail, Name, Email} ->
    Key = string:concat(string:concat("book:", Name), ":emails"),
    {ok, _Reply} = eredis:q(Conn, ["SREM", Key, Email]),
    {reply, ok, Conn};
  {removePhone, Name, Phone} ->
    Key = string:concat(string:concat("book:", Name), ":phones"),
    {ok, _Reply} = eredis:q(Conn, ["SREM", Key, Phone]),
    {reply, ok, Conn};
  {getUsers} ->
    {ok, Reply} = eredis:q(Conn, ["SMEMBERS", "book"]),
    {reply, Reply, Conn};
  {getEmails, Name} ->
    Key = string:concat(string:concat("book:", Name), ":emails"),
    {ok, Reply} = eredis:q(Conn, ["SMEMBERS", Key]),
    {reply, Reply, Conn};
  {getPhones, Name} ->
    Key = string:concat(string:concat("book:", Name), ":phones"),
    {ok, Reply} = eredis:q(Conn, ["SMEMBERS", Key]),
    {reply, Reply, Conn}
  end
.

handle_cast(stop, Conn) ->
  {stop, normal, Conn}
.

add_user(Name) ->
  gen_server:call(address_book, {addUser, Name})
.

add_email(Name, Email) ->
  gen_server:call(address_book, {addEmail, Name, Email})
.

add_phone(Name, Phone) ->
  gen_server:call(address_book, {addPhone, Name, Phone})
.

remove_email(Name, Email) ->
  gen_server:call(address_book, {addEmail, Name, Email})
.

remove_phone(Name, Phone) ->
  gen_server:call(address_book, {addPhone, Name, Phone})
.

get_users() ->
  gen_server:call(address_book, {getUsers})
.

get_emails(Name) ->
  gen_server:call(address_book, {getEmails, Name})
.

get_phones(Name) ->
  gen_server:call(address_book, {getPhones, Name})
.
