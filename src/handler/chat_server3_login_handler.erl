%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 八月 2018 2:50 AM
%%%-------------------------------------------------------------------
-module(chat_server3_login_handler).
-author("ubuntu").

%% API
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    HasBody = cowboy_req:has_body(Req0),
    Req = maybe_echo(Method, HasBody, Req0),
    {ok, Req, Opts}.

maybe_echo(<<"POST">>, true, Req0) ->
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    Name = element(2, lists:keyfind(<<"username">>, 1, PostVals)),
    Password = element(2, lists:keyfind(<<"password">>, 1, PostVals)),
    Token = chat_server3_utils:product_token(),
    Flag = check_login(Name, Password, Token),
    case Flag of
        repeatlogin ->
            Echo = "/repeatlogin",
            cowboy_req:reply(302, #{<<"Location">> => list_to_binary(Echo)}, Req);
        false ->
            Echo = "/loginfail",
            cowboy_req:reply(302, #{<<"Location">> => list_to_binary(Echo)}, Req);
        _ ->
            Req1 = cowboy_req:set_resp_cookie(<<"token">>, Token, Req, #{path => <<"/">>}),
            Req2 = cowboy_req:set_resp_cookie(<<"user">>, Name, Req1, #{path => <<"/">>}),
            Echo = "/index",
            echo(Echo,Req2)
    end;
maybe_echo(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
    cowboy_req:reply(405, Req).

echo(Echo,Req) ->
    %% 页面跳转
    cowboy_req:reply(
        302,
        #{<<"Location">> => list_to_binary(Echo)},
        Req
    ).

%% @doc 验证登录
check_login(UserName, Password, Token) ->
    [H|_] = chat_server3_mnesia:select_user(UserName),
    case Password =:= H of
        true ->
            %% 将token和当前登录用户绑定起来
            case whereis(binary_to_atom(UserName, utf8)) == undefined of
                true ->
                    chat_server3_ets:insert(player, Token, UserName);
                false ->
                    repeatlogin
            end;
        false ->
            false
    end.
