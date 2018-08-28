%%%-------------------------------------------------------------------
%% @doc chat_server3 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server3_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    chat_server3_mnesia:create(),
    chat_server3_ets:make_new_ets(),
    SupFlags = {one_for_one, 3, 10},

    AChild = {tag1, {chat_server3_room_sup, start_link, []},
        permanent, 10000, supervisor, [chat_server3_room_sup]},

    BChild = {tag2, {chat_server3_player_sup, start_link, []},
        permanent, 10000, supervisor, [chat_server3_player_sup]},

    {ok, {SupFlags, [AChild, BChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
