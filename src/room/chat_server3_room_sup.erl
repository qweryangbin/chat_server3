%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 八月 2018 3:17 AM
%%%-------------------------------------------------------------------
-module(chat_server3_room_sup).
-author("ubuntu").

-behaviour(supervisor).
%% API
-export([start_link/0,
    create_room/1,
    delete_room/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc 创建群聊房间进程
-spec create_room(Name::atom()) -> ok.
create_room(Name) ->
    supervisor:start_child(chat_server3_room_sup, [Name]).

delete_room(Name) ->
    Pid = whereis(Name),
    supervisor:terminate_child(chat_server3_room_sup, Pid),
    supervisor:delete_child(chat_server3_room_sup, Pid).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    SupFlags = {simple_one_for_one, 1, 3},

    AChild = {tag1, {chat_server3_room_wk, start_link, []},
        permanent, 10000, worker, [chat_server3_room_wk]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
