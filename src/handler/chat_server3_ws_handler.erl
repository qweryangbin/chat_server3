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
    {_, User, Target, Text} = msg_pb:decode_msg(Msg, 'SendMessageRequest'),
    case Target == <<"all">> of
        true ->
            chat_server3_room_sup:create_room(binary_to_atom(Text, utf8)),
            Date = calendar:local_time(),
            chat_server3_mnesia:add_room(binary_to_atom(Text, utf8), Date),
            Room = #'SendMessageRequest'{sender = "roomadd", receiver = "all", text = binary_to_list(Text)},
            EncodeRoom = msg_pb:encode_msg(Room),
            chat_server3_player_wk:push_one_room(binary_to_atom(User, utf8), EncodeRoom);
        false ->
            case Target == <<"getUser">> of
                true ->
                    {_, Token} = lists:keyfind(<<"token">>, 1, State),
                    UserName = chat_server3_ets:lookup(player, binary_to_list(Token)),
                    chat_server3_player_wk:save_client(binary_to_atom(UserName, utf8), {self(), Token}),
                    push_all_user(UserName),
                    push_all_room(UserName);
                false ->
                    case Target == <<"addUser">> of
                        true ->
                            Name = binary_to_atom(User, utf8),
                            chat_server3_mnesia:add_room_user(Name, binary_to_atom(Text, utf8));
                        false ->
                            case Target == <<"room">> of
                                true ->
                                    RoomName = binary_to_atom(User, utf8),
                                    erlang:link(whereis(RoomName)),
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
                                    end;
                                false ->
                                    case Target == <<"roomUser">> of
                                        true ->
                                            CurrentUser = list_to_atom(string:sub_word(binary_to_list(User), 2, $:)),
                                            CurrentRoomName = list_to_atom(string:sub_word(binary_to_list(User), 1, $:)),
                                            Bin = unicode:characters_to_list(Text),
                                            Date = calendar:local_time(),
                                            chat_server3_mnesia:add_messages(CurrentRoomName, CurrentUser, Bin, Date),
                                            chat_server3_room_wk:send_room_msg(CurrentRoomName, Msg);
                                        false ->
                                            case Target == <<"exit">> of
                                                true ->
                                                    CurrentUser = string:sub_word(binary_to_list(User), 2, $:),
                                                    CurrentRoomName = list_to_atom(string:sub_word(binary_to_list(User), 1, $:)),
                                                    chat_server3_room_wk:send_user_quit_room_msg(CurrentRoomName, Msg, self()),
                                                    chat_server3_mnesia:remove_room_user(CurrentRoomName, CurrentUser),
                                                    RoomUserList = chat_server3_mnesia:select_room_user(),
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
                                                            exit(shutdown)
                                                    end;
                                                false ->
                                                    chat_server3_player_wk:send(binary_to_atom(Target, utf8), Msg)
                                            end
                                    end
                            end
                    end
            end
    end,
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info({send, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info({push_user, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info({push_room, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info({push_one_room, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info({push_room_user, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info({send_room_msg, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info({update_room, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info({push_room_msg, Msg}, State) ->
    {reply, {binary, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.

push_all_user(UserName) ->
    UserList = ets:select(player, [{{'$1', '$2'}, [], ['$2']}]),
    UserList2 = lists:foldl(fun(E, S) -> S ++ binary_to_list(E) ++ "," end, ",", UserList),
    Users = #'SendMessageRequest'{sender = "system", receiver = "all", text = UserList2},
    EncodeUserList = msg_pb:encode_msg(Users),
    chat_server3_player_wk:push_user_list(binary_to_atom(UserName, utf8), EncodeUserList).

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

push_all_room_user(Name, PidList) ->
    RoomUserList = chat_server3_mnesia:select_room_user(),
    RoomUserList2 = lists:foldl(fun(E, S) -> S ++ atom_to_list(E) ++ "," end, ",", RoomUserList),
    RoomUsers = #'SendMessageRequest'{sender = "room", receiver = "all", text = RoomUserList2},
    EncodeRoomUserList = msg_pb:encode_msg(RoomUsers),
    chat_server3_room_wk:push_room_user_list(Name, EncodeRoomUserList, PidList).

update_room(UserName, CurrentRoomName) ->
    Rooms = #'SendMessageRequest'{sender = "update", receiver = "all", text = atom_to_list(CurrentRoomName)},
    EncodeRoomList = msg_pb:encode_msg(Rooms),
    chat_server3_player_wk:update_room_list(list_to_atom(UserName), EncodeRoomList).

push_offline_message(RoomName, Pid) ->
    Messages = chat_server3_mnesia:select_room_message(RoomName),
    NewMessages = chat_server3_utils:to_string(Messages),
    NewMessages1 = chat_server3_utils:to_list(NewMessages),
    NewMessages2 = lists:foldl(fun(E, S) -> S ++ E ++ "," end, ",", NewMessages1),
    Msg = #'SendMessageRequest'{sender = "showmessage", receiver = "showmessage", text = NewMessages2},
    EncodeMsg = msg_pb:encode_msg(Msg),
    chat_server3_room_wk:push_offline_message(RoomName, Pid, EncodeMsg).