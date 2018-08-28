%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 八月 2018 7:29 PM
%%%-------------------------------------------------------------------
-module(chat_server3_mnesia).
-author("ubuntu").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([create/0,
    add_user/2,
    selct_all_user/0,
    select_user/1,
    init_tables/0,
    add_messages/4,
    select_room_message/1,
    remove_room_message/1,
    select_room_message_all_row/1]).
-record(users, {username, password}).
-record(messages, {roomname, username, text, time}).

%% 创建数据库
create() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(users, [{attributes, record_info(fields, users)}, {disc_copies, [node()]}]),
    mnesia:create_table(messages, [{attributes, record_info(fields, messages)}, {disc_copies, [node()]}, {type, bag}]).

%% 初始化表数据
init_tables() ->
    F = fun() ->
        lists:foreach(fun(Item) -> mnesia:write(Item) end,example_tables())
        end,
    mnesia:transaction(F).

example_tables() ->
    [
        {users, <<"zhangsan">>, <<"1234">>},
        {users, <<"lisi">>, <<"1234">>},
        {users, <<"root">>, <<"root">>},
        {users, <<"yangb">>, <<"123">>},
        {users, <<"wangwu">>, <<"123">>},
        {users, <<"zhaoliu">>, <<"123456">>}
    ].

%% 添加用户
-spec add_user(UserName::term(), Password::term()) ->term().
add_user(UserName, Password) ->
    Row = #users{username = UserName, password = Password},
    F = fun() ->
        mnesia:write(Row)
        end,
    mnesia:transaction(F).

add_messages(RoomName, UserName, Text, Date) ->
    Row = #messages{roomname = RoomName, username = UserName, text = Text, time = Date},
    F = fun() ->
        mnesia:write(Row)
        end,
    mnesia:transaction(F).

%% 查询所有用户
selct_all_user() ->
    do(qlc:q([X || X <- mnesia:table(users)])).

select_user(UserName) ->
    do(qlc:q([X#users.password || X <- mnesia:table(users), X#users.username =:= UserName])).

%% 查询某个群聊房间的消息记录（指定列）
select_room_message(RoomName) ->
    do(qlc:q([{X#messages.username, X#messages.text} || X <- mnesia:table(messages), X#messages.roomname =:= RoomName])).

select_room_message_all_row(RoomName) ->
    do(qlc:q([X || X <- mnesia:table(messages), X#messages.roomname =:= RoomName])).

%% 删除群聊消息记录
remove_room_message(Item) ->
    Oid = {messages,Item},
    F = fun() ->
        mnesia:delete(Oid)
        end,
    mnesia:transaction(F).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.




