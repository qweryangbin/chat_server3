%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2018 1:17 AM
%%%-------------------------------------------------------------------
-module(chat_server3_utils).
-author("ubuntu").

%% API
-export([product_token/0,
    to_string/1,
    to_list/1]).

product_token() ->
    L = os:cmd(uuidgen) -- "\n",
    L.


to_string(AtomList) when is_list(AtomList) ->
    to_string(AtomList,"");
to_string(_) ->
    {error,error_type}.
to_string([], R) -> lists:reverse(R);
to_string([H|T], R) ->
    to_string(T,tuple_to_list(H) ++ R);
to_string([H|T], R) when is_list(H) ->
    to_string(T,H ++ R);
to_string(_, _) ->
    {error,error_type}.


to_list(L) when is_list(L) ->
    to_list(L, []);
to_list(_) ->
    {error, error_type}.
to_list([], R) -> lists:reverse(R);
to_list([H|T], R) ->
    case is_list(H) of
        true ->
            to_list(T, [H|R]);
        false ->
            L4 = atom_to_list(H),
            to_list(T, [L4|R])
    end.