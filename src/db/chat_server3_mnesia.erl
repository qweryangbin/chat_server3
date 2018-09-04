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
    select_room_message_all_row/1,
    remove_room/1,
    add_room/2,
    select_room/0,
    add_room_user/2,
    select_room_user/1,
    remove_room_user/2,
    remove_room_all_user/1,
    my_cursor/1,
    my_next/2,
    add_private_msg/5,
    select_private_msg/1,
    update_msg_type/1,
    select_all_prviate_msg/1]).

-record(users, {username, password}).
-record(messages, {roomname, username, text, time}).
-record(room, {roomname, time}).
-record(roomuser, {roomname, username}).
-record(privatemessages, {sender, receiver, msg, type, time}).

%% @doc 创建数据库
create() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(users, [{attributes, record_info(fields, users)}, {disc_copies, [node()]}]),
    mnesia:create_table(room, [{attributes, record_info(fields, room)}, {disc_copies, [node()]}]),
    mnesia:create_table(roomuser, [{attributes, record_info(fields, roomuser)}, {disc_copies, [node()]}, {type, bag}]),
    mnesia:create_table(messages, [{attributes, record_info(fields, messages)}, {disc_copies, [node()]}, {type, bag}]),
    mnesia:create_table(privatemessages, [{attributes, record_info(fields,privatemessages)}, {disc_copies, [node()]}, {type, bag}]).

%% @doc 初始化表数据
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

%% @doc 添加用户
-spec add_user(UserName::term(), Password::term()) ->term().
add_user(UserName, Password) ->
    Row = #users{username = UserName, password = Password},
    F = fun() ->
        mnesia:write(Row)
        end,
    mnesia:transaction(F).

%% @doc 存储离线消息
add_messages(RoomName, UserName, Text, Date) ->
    Row = #messages{roomname = RoomName, username = UserName, text = Text, time = Date},
    F = fun() ->
        mnesia:write(Row)
        end,
    mnesia:transaction(F).

%% @doc 查询所有用户
selct_all_user() ->
    do(qlc:q([X || X <- mnesia:table(users)])).

%% @doc 查询指定用户的相关数据
select_user(UserName) ->
    do(qlc:q([X#users.password || X <- mnesia:table(users), X#users.username =:= UserName])).

%% @doc 查询某个群聊房间的消息记录（指定列）
select_room_message(RoomName) ->
    do(qlc:q([{X#messages.username, X#messages.text} || X <- mnesia:table(messages), X#messages.roomname =:= RoomName])).

%% @doc 查询某个群聊房间的消息记录（所有列）
select_room_message_all_row(RoomName) ->
    do(qlc:q([X || X <- mnesia:table(messages), X#messages.roomname =:= RoomName])).

%% @doc 删除群聊消息记录
remove_room_message(Item) ->
    Oid = {messages,Item},
    F = fun() ->
        mnesia:delete(Oid)
        end,
    mnesia:transaction(F).

%% @doc 添加群聊房间
add_room(RoomName, Date) ->
    Row = #room{roomname = RoomName, time = Date},
    F = fun() ->
        mnesia:write(Row)
        end,
    mnesia:transaction(F).

%% @doc 添加群聊房间用户
add_room_user(RoomName, UserName) ->
    Row = #roomuser{roomname = RoomName, username = UserName},
    F = fun() ->
        mnesia:write(Row)
        end,
    mnesia:transaction(F).

%% @doc 查询指定群聊房间名字
select_room() ->
    do(qlc:q([X#room.roomname || X <- mnesia:table(room)])).

select_room_user(Name) ->
    do(qlc:q([X#roomuser.username || X <- mnesia:table(roomuser), X#roomuser.roomname == Name])).

%% @doc 删除群聊房间
remove_room(Item) ->
    Oid = {room, Item},
    F = fun() ->
        mnesia:delete(Oid)
        end,
    mnesia:transaction(F).

%% @doc 删除群聊房间指定用户
remove_room_user(RoomName, UserName) ->
    Oid = {roomuser, RoomName, list_to_atom(UserName)},
    F = fun() ->
        mnesia:delete_object(Oid)
        end,
    mnesia:transaction(F).

%% @doc 删除群聊房间所有用户
remove_room_all_user(RoomName) ->
    Oid = {roomuser, RoomName},
    F = fun() ->
        mnesia:delete(Oid)
        end,
    mnesia:transaction(F).

%% @doc 实现分页查询
my_cursor(Name) ->
    Q = qlc:q([X || X<- mnesia:table(Name)]),
    mnesia:activity(async_dirty, fun() -> qlc:cursor(Q, []) end, mnesia_frag).

my_next(Cursor, Num) ->
    Get = fun() ->
          qlc:next_answers(Cursor, Num)
          end,
    mnesia:activity(async_dirty, Get, mnesia_frag).

%% @doc 存储私聊消息
add_private_msg(Sender, Receiver, Msg, Type, Time) ->
    Row = #privatemessages{sender = Sender, receiver = Receiver, msg = Msg, type = Type, time = Time},
    F = fun() ->
        mnesia:write(Row)
        end,
    mnesia:transaction(F).

%% @doc 查询私聊离线消息
select_private_msg(UserName) ->
    F = fun() ->
        Q = qlc:q([{X#privatemessages.sender, X#privatemessages.type, X#privatemessages.msg, X#privatemessages.time} || X <- mnesia:table(privatemessages), (X#privatemessages.receiver =:= UserName) or (X#privatemessages.sender =:= UserName)]),
        qlc:e(qlc:keysort(4, Q, [{order, ascending}]))
        end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% @doc 查询所有私聊离线消息
select_all_prviate_msg(UserName) ->
    do(qlc:q([X || X <- mnesia:table(privatemessages), X#privatemessages.receiver =:= UserName])).

%% @doc 更改离线消息为已读状态
update_msg_type(Msg) ->
    {_, Sender, Receiver, NewMsg, _, Time} = Msg,
    NewMsg2 = #privatemessages{sender = Sender, receiver = Receiver, msg = NewMsg, type = <<"on-line">>, time = Time},
    F = fun() ->
        mnesia:delete_object(Msg),
        mnesia:write(NewMsg2)
        end,
    mnesia:transaction(F).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.





