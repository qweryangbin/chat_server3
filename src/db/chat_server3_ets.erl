%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2018 6:26 PM
%%%-------------------------------------------------------------------
-module(chat_server3_ets).
-author("ubuntu").

%% API
-export([make_new_ets/0,
    insert/3,
    lookup/2,
    delete/2,
    match_delete/2,
    delete_tab/1,
    insert_webprocess/4]).

%% @doc 创建ets表
make_new_ets() ->
    ets:new(player, [set, public, named_table]),
    ets:new(webprocess, [bag, public, named_table]).

%% @doc 往表中插入数据
insert(TabName, Key, Value) ->
    ets:insert(TabName, {Key, Value}).

%% @doc 往webprocess表中插入数据
insert_webprocess(TabName, RoomName, UserName, Pid) ->
    ets:insert(TabName, {RoomName, UserName, Pid}).

%% @doc 查询数据
lookup(TabName,Key) ->
    [L|_]= ets:lookup(TabName, Key),
    {_, UserName} = L,
    UserName.

%% @doc 删除表中数据
delete(TabId, Object) ->
    ets:delete(TabId, Object).

%% @doc 删除指定数据
match_delete(Name, Pattern) ->
    ets:match_delete(Name, {'$1', Pattern}).

%% @doc 删除表
delete_tab(Name) ->
    ets:delete(Name).



