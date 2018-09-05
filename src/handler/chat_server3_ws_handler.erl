-module(chat_server3_ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-include("msg_pb.hrl").

init(Req, State) ->
    Cookies = cowboy_req:parse_cookies(Req),
    Token = lists:keyfind(<<"token">>, 1, Cookies),
    User = element(2, lists:keyfind(<<"user">>, 1, Cookies)),
    chat_server3_player_sup:create_player(binary_to_atom(User, utf8)),
    {cowboy_websocket, Req, [Token|State], #{idle_timeout => 1000000}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({binary, Msg}, State) ->
    {_, User, Target, Text, Type} = msg_pb:decode_msg(Msg, 'SendMessageRequest'),
    case Target of
        <<"all">> ->
            add_room(User, Text);
        <<"getUser">> ->
            init_show(State);
        <<"addUser">> ->
            add_room_user(User, Text);
        <<"room">> ->
            init_chat_room_user(User);
        <<"roomUser">> ->
            send_chat_room_msg(User, Text, Msg);
        <<"exit">> ->
            user_quit_room(User, Msg);
        <<"deleteroom">> ->
            delete_chat_room(User, Text);
        <<"userexit">> ->
            user_login_out(User, Msg);
        <<"loadMsg">> ->
            load_private_msg(User);
        <<"updateMsgType">> ->
            MsgList = chat_server3_mnesia:select_all_prviate_msg(User),
            [chat_server3_mnesia:update_msg_type(Msg) || Msg <- MsgList];
        _ ->
            send_private_msg(User,Target,Text, Msg, Type)
    end,
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info({_Request, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.

%% @doc 创建群聊房间
add_room(User, Text) ->
    chat_server3_room_sup:create_room(binary_to_atom(Text, utf8)),
    Date = calendar:local_time(),
    chat_server3_mnesia:add_room(binary_to_atom(Text, utf8), Date),
    Room = #'SendMessageRequest'{sender = "roomadd", receiver = "all", text = binary_to_list(Text)},
    EncodeRoom = msg_pb:encode_msg(Room),
    chat_server3_player_wk:push_one_room(binary_to_atom(User, utf8), EncodeRoom).

%% @doc 初始化页面显示
init_show(State) ->
    {_, Token} = lists:keyfind(<<"token">>, 1, State),
    UserName = chat_server3_ets:lookup(player, binary_to_list(Token)),
    chat_server3_player_wk:save_client(binary_to_atom(UserName, utf8), {self(), Token}),
    push_all_user(UserName),
    push_all_room(UserName).

%% @doc 添加群聊房间用户
add_room_user(User, Text) ->
    Name = binary_to_atom(User, utf8),
    chat_server3_mnesia:add_room_user(Name, binary_to_atom(Text, utf8)).

%% @doc 初始化群聊房间页面显示
init_chat_room_user(User) ->
    RoomName = binary_to_atom(User, utf8),
    chat_server3_ets:insert(webprocess, RoomName, self()),
    RoomProcessList = ets:match_object(webprocess, {RoomName, '_'}),
    NewRoomProcessList = [X || {_, X} <- RoomProcessList],
    push_all_room_user(RoomName, NewRoomProcessList),
    Messages = chat_server3_mnesia:select_room_message(RoomName),
    case length(Messages) > 0 of
        true ->
            push_offline_message(RoomName, self());
        false ->
            ok
    end.

%% @doc 发送群聊房间消息
send_chat_room_msg(User, Text, Msg) ->
    CurrentUser = list_to_atom(string:sub_word(binary_to_list(User), 2, $:)),
    CurrentRoomName = list_to_atom(string:sub_word(binary_to_list(User), 1, $:)),
    Bin = unicode:characters_to_list(Text),
    Date = calendar:now_to_universal_time(erlang:now()),
    Time = calendar:datetime_to_gregorian_seconds(Date),
    chat_server3_mnesia:add_messages(CurrentRoomName, atom_to_binary(CurrentUser, utf8), Bin, Time),
    chat_server3_room_wk:send_room_msg(CurrentRoomName, Msg).

%% @doc 用户退出群聊房间
user_quit_room(User, Msg) ->
    CurrentUser = string:sub_word(binary_to_list(User), 2, $:),
    CurrentRoomName = list_to_atom(string:sub_word(binary_to_list(User), 1, $:)),
    chat_server3_room_wk:send_user_quit_room_msg(CurrentRoomName, Msg, self()),
    chat_server3_mnesia:remove_room_user(CurrentRoomName, CurrentUser),
    RoomUserList = chat_server3_mnesia:select_room_user(CurrentRoomName),
    case length(RoomUserList) > 0 of
        true ->
            RoomProcessList = ets:match_object(webprocess, {CurrentRoomName, '_'}),
            NewRoomProcessList = [X || {_, X} <- RoomProcessList],
            push_all_room_user(CurrentRoomName, NewRoomProcessList);
        false ->
            chat_server3_mnesia:remove_room(CurrentRoomName),
            Messages = chat_server3_mnesia:select_room_message_all_row(CurrentRoomName),
            update_room(CurrentUser, CurrentRoomName),
            case length(Messages) > 0  of
                true ->
                    chat_server3_mnesia:remove_room_message(CurrentRoomName);
                false ->
                    ok
            end,
            chat_server3_room_sup:delete_room(CurrentRoomName)
    end.

%% @doc 删除群聊房间
delete_chat_room(User, Text) ->
    CurrentRoomName = binary_to_atom(User, utf8),
    CurrentUser = binary_to_atom(Text, utf8),
    chat_server3_mnesia:remove_room(CurrentRoomName),
    chat_server3_mnesia:remove_room_all_user(CurrentRoomName),
    update_room(atom_to_list(CurrentUser), CurrentRoomName),
    chat_server3_room_sup:delete_room(CurrentRoomName).

%% @doc 用户退出登录
user_login_out(User, Msg) ->
    chat_server3_ets:match_delete(player,User),
    chat_server3_player_wk:send_user_login_out_msg(binary_to_atom(User, utf8), Msg),
    chat_server3_player_sup:delete_palyer(binary_to_atom(User, utf8)).

%% @doc 发送私聊消息
send_private_msg(User,Target,Text, Msg, Type) ->
    Bin = unicode:characters_to_list(Text),
    Date = calendar:now_to_universal_time(erlang:now()),
    Time = calendar:datetime_to_gregorian_seconds(Date),
    case Type == <<"off-line">> of
        true ->
            chat_server3_mnesia:add_private_msg(User,Target, Bin, Type, Time);
        false ->
            chat_server3_mnesia:add_private_msg(User,Target, Bin, Type, Time),
            chat_server3_player_wk:send(binary_to_atom(Target, utf8), Msg)
    end.

%% @doc 加载载私聊离线消息
load_private_msg(UserName) ->
    PrivateMsgList = chat_server3_mnesia:select_private_msg(UserName),
    case length(PrivateMsgList) > 0 of
        true ->
            NewMessages = chat_server3_utils:to_string(PrivateMsgList),
            NewMessages1 = chat_server3_utils:to_list1(NewMessages),
            NewMessages2 = lists:foldl(fun(E, S) -> S ++ E ++ "," end, ",", NewMessages1),
            MsgList = #'SendMessageRequest'{receiver = "privatemsg", text = NewMessages2},
            EncodeMsgList = msg_pb:encode_msg(MsgList),
            chat_server3_player_wk:push_user_private_msg(binary_to_atom(UserName, utf8), EncodeMsgList);
        false ->
            ok
    end.

%% @doc 推送当前在线玩家
-spec push_all_user(UserName::binary()) -> ok.
push_all_user(UserName) ->
    UserList = ets:select(player, [{{'$1', '$2'}, [], ['$2']}]),
    UserList2 = lists:foldl(fun(E, S) -> S ++ binary_to_list(E) ++ "," end, ",", UserList),
    Users = #'SendMessageRequest'{sender = "system", receiver = "all", text = UserList2},
    EncodeUserList = msg_pb:encode_msg(Users),
    chat_server3_player_wk:push_user_list(binary_to_atom(UserName, utf8), EncodeUserList).

%% @doc 推送当前已经创建的群聊房间
-spec push_all_room(UserName::binary()) -> ok.
push_all_room(UserName) ->
    RoomList = chat_server3_mnesia:select_room(),
    case RoomList == "" of
        true ->
            ok;
        false ->
            [chat_server3_room_sup:create_room(X) || X <- RoomList],
            RoomList2 = lists:foldl(fun(E, S) -> S ++ atom_to_list(E) ++ "," end, ",", RoomList),
            Rooms = #'SendMessageRequest'{sender = "room", receiver = "all", text = RoomList2},
            EncodeRoomList = msg_pb:encode_msg(Rooms),
            chat_server3_player_wk:push_room_list(binary_to_atom(UserName, utf8), EncodeRoomList)
    end.

%% @doc 推送群聊房间的用户
-spec push_all_room_user(UserName::list(), PidList::list()) -> ok.
push_all_room_user(Name, PidList) ->
    RoomUserList = chat_server3_mnesia:select_room_user(Name),
    RoomUserList2 = lists:foldl(fun(E, S) -> S ++ atom_to_list(E) ++ "," end, ",", RoomUserList),
    RoomUsers = #'SendMessageRequest'{sender = "room", receiver = "all", text = RoomUserList2},
    EncodeRoomUserList = msg_pb:encode_msg(RoomUsers),
    chat_server3_room_wk:push_room_user_list(Name, EncodeRoomUserList, PidList).

%% @doc 更新群聊房间
-spec update_room(UserName::list(), CurrentRoomName::atom()) -> ok.
update_room(UserName, CurrentRoomName) ->
    Rooms = #'SendMessageRequest'{sender = "update", receiver = "all", text = atom_to_list(CurrentRoomName)},
    EncodeRoomList = msg_pb:encode_msg(Rooms),
    chat_server3_player_wk:update_room_list(list_to_atom(UserName), EncodeRoomList).

%% @doc 推送群聊房间离线消息
-spec push_offline_message(UserName::atom(), Pid::pid()) -> ok.
push_offline_message(RoomName, Pid) ->
    Messages = chat_server3_mnesia:select_room_message(RoomName),
    NewMessages = chat_server3_utils:to_string(Messages),
    NewMessages1 = chat_server3_utils:to_list1(NewMessages),
    NewMessages2 = lists:foldl(fun(E, S) -> S ++ E ++ "," end, ",", NewMessages1),
    Msg = #'SendMessageRequest'{sender = "showmessage", receiver = "showmessage", text = NewMessages2},
    EncodeMsg = msg_pb:encode_msg(Msg),
    chat_server3_room_wk:push_offline_message(RoomName, Pid, EncodeMsg).

