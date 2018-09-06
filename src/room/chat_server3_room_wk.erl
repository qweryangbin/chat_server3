%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 八月 2018 3:19 AM
%%%-------------------------------------------------------------------
-module(chat_server3_room_wk).
-author("ubuntu").

-behaviour(gen_server).

%% API
-export([start_link/0,
    start_link/1,
    push_room_user_list/3,
    send_user_quit_room_msg/3,
    push_offline_message/3,
    send_room_msg/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {room}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 推送群聊房间用户
-spec push_room_user_list(RoomName::atom(), Msg::binary(), PidList::list()) -> ok.
push_room_user_list(RoomName, Msg, PidList)->
    call(RoomName, {push_room_user, Msg, PidList}).

%% @doc 用户退出群聊房间提示
-spec send_user_quit_room_msg(RoomName::atom(), Msg::binary(), Pid::pid()) -> ok.
send_user_quit_room_msg(RoomName, Msg, Pid) ->
    timer:sleep(1000),
    call(RoomName, {send_user_quit_room_msg, RoomName, Msg, Pid}).

%% @doc 发送群聊消息
-spec send_room_msg(RoomName::atom(), Msg::binary()) -> ok.
send_room_msg(RoomName, Msg) ->
    call(RoomName, {send_room_msg, RoomName, Msg}).

%% @doc 推送群聊房间离线消息
-spec push_offline_message(RoomName::atom(), Pid::pid(), Msg::binary()) -> ok.
push_offline_message(RoomName, Pid, Msg) ->
    call(RoomName, {show_room_message, Pid, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_link(A::atom()) -> ok.
start_link(A) ->
    gen_server:start_link({local, A}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    timer:send_interval(10000,self(),check),
    timer:send_interval(5000, self(), is_live),
    {ok, #state{room = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

%% @doc 推送当前群聊房间用户
handle_call({push_room_user, Msg, PidList}, _From, State) ->
    [Pid ! {push_room_user, Msg} || Pid <- PidList],
    {reply, ok, State};

%% @doc 推送群成员退出群聊房间提示
handle_call({send_user_quit_room_msg, RoomName, Msg, Pid}, _From, State) ->
    RoomProcessList = ets:match_object(webprocess, {RoomName, '_'}),
    chat_server3_ets:match_delete(webprocess, Pid),
    [Pid ! {send_room_msg, Msg} || {_, Pid} <- RoomProcessList],
    {reply, ok, State};

%% @doc 发送群聊房间消息
handle_call({send_room_msg, RoomName, Msg}, _From, State) ->
    RoomProcessList = ets:match_object(webprocess, {RoomName, '_', '_'}),
    [Pid ! {send_room_msg, Msg} || {_, _, Pid} <- RoomProcessList],
    {reply, ok, State};

%% @doc 显示群聊离线消息
handle_call({show_room_message, Pid, Msg}, _From, State) ->
    Pid ! {push_room_msg, Msg},
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

%% @doc 玩家全部退出后房间进程也必须得全部结束
handle_info(check, State) ->
    PlayerList = supervisor:which_children(chat_server3_player_sup),
    case length(PlayerList) > 0 of
        true ->
            ok;
        false ->
            RoomList = supervisor:which_children(chat_server3_room_sup),
            [delete_room(X) || X <- RoomList]
    end,
    {noreply,State};

%% @doc 检测某个玩家的群聊websocket进程是否还活着
handle_info(is_live, State) ->
    RoomProcessList = ets:match_object(webprocess, {'_', '_', '_'}),
    [remove_webprocess(X) || X <- RoomProcessList],
    {noreply,State};

handle_info(_Request, State) ->
    {noreply,State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec call(RoomName::atom(), Msg::binary()) -> ok.
call(RoomName, Msg) ->
    gen_server:call(RoomName, Msg).

delete_room(X) ->
    {_, Pid, _, _} = X,
    {_, RegName} = erlang:process_info(Pid, registered_name),
    chat_server3_room_sup:delete_room(RegName).

remove_webprocess(X) ->
    {_, _, Pid} = X,
    Flag = erlang:is_process_alive(Pid),
    case Flag of
        true ->
            ok;
        false ->
            ets:match_delete(webprocess, X)
    end.
