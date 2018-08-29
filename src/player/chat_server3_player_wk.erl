%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 八月 2018 3:18 AM
%%%-------------------------------------------------------------------
-module(chat_server3_player_wk).
-author("ubuntu").

-behaviour(gen_server).

%% API
-export([start_link/0,
    start_link/1,
    push_user_list/2,
    save_client/2,
    send/2,
    push_room_list/2,
    push_one_room/2,
    push_room_user_list/3,
    update_room_list/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {client}).
-include("msg_pb.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec push_user_list(User::atom(), List::list()) -> ok.
push_user_list(User, List) ->
    call(User, {push_user, List}).

-spec save_client(User::atom(), Client::tuple()) -> ok.
save_client(User, Client) when is_tuple(Client) ->
    call(User, {save_client, Client}).

-spec send(User::atom(), Mag::binary()) -> ok.
send(User, Msg) ->
    call(User, {send_to_one, Msg}).

-spec push_room_list(User::atom(), List::list()) -> ok.
push_room_list(User, List) ->
    call(User, {push_room, List}).

-spec push_one_room(User::atom(), Msg::binary()) -> ok.
push_one_room(User, Msg) ->
    call(User, {push_one_room, Msg}).

-spec push_room_user_list(User::atom(), List::list(), Pid::pid()) -> ok.
push_room_user_list(User, List, Pid) ->
    call(User, {push_room_user, List, Pid}).

-spec update_room_list(UserName::atom(), Msg::binary()) -> ok.
update_room_list(UserName, Msg) ->
    call(UserName, {update_room, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
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
    {ok, #state{client = []}}.

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
handle_call({push_user, List}, _From, State) ->
    TokenList = get_token_list(),
    [list_to_atom(X) ! {push_user, List} || X <- TokenList],
    {reply, ok, State};
handle_call({send_to_one, Msg}, _From, State) ->
    {_, _, Target, _} = msg_pb:decode_msg(Msg, 'SendMessageRequest'),
    Token = ets:select(player, [{{'$1', Target}, [], ['$1']}]),
    [list_to_atom(X) ! {send, Msg} || X <- Token],
    {reply, ok, State};
handle_call({push_room, Msg}, _From, State) ->
    TokenList = get_token_list(),
    [list_to_atom(X) ! {push_room, Msg} || X <- TokenList],
    {reply, ok, State};
handle_call({push_one_room, Msg}, _From, State) ->
    TokenList = get_token_list(),
    [list_to_atom(X) ! {push_one_room, Msg} || X <- TokenList],
    {reply, ok, State};
handle_call({push_room_user, Msg, Pid}, _From, State) ->
    Pid ! {push_room_user, Msg},
    {reply, ok, State};
handle_call({update_room, Msg}, _From, State) ->
    TokenList = get_token_list(),
    [list_to_atom(X) ! {push_one_room, Msg} || X <- TokenList],
    {reply, ok, State};
handle_call({save_client, {Pid, Name}}, _From, #state{client = Client} = State) ->
    L = registered(),
    NewName = binary_to_atom(Name, utf8),
    case lists:member(NewName, L) of
        true ->
            {reply, false, State};
        false ->
            %% 防止页面刷新时再次注册这个进程
            case lists:member(NewName, Client) of
                true ->
                    {reply, true, State};
                false ->
                    register(NewName, Pid),
                    NewState = State#state{client = [NewName|Client]},
                    {reply, true, NewState}
            end
    end.

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
handle_info(_Info, State) ->
    {noreply, State}.

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
%% @doc 拿到所有玩家的token列表
get_token_list() ->
    TokenList = ets:select(player, [{{'$1', '$2'}, [], ['$1']}]),
    TokenList.

-spec call(UserName::atom(), Msg::binary()) -> ok.
call(UserName, Msg) ->
    gen_server:call(UserName, Msg).