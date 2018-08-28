%%%-------------------------------------------------------------------
%% @doc chat_server3 public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server3_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/index", cowboy_static, {priv_file, chat_server3, "index.html"}},
            {"/websocket", chat_server3_ws_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, chat_server3, "static"}},
            {"/proto/[...]", cowboy_static, {priv_dir, chat_server3, "proto"}},
            {"/login", cowboy_static, {priv_file, chat_server3, "login.html"}},
            {"/submit", chat_server3_login_handler, []},
            {"/room", cowboy_static, {priv_file, chat_server3, "room.html"}}
        ]}
    ]),
    {ok, Port} = application:get_env(http_port),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    chat_server3_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
