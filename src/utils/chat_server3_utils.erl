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
    index/2,
    to_list1/1]).

%% @doc 产生token
product_token() ->
    L = os:cmd(uuidgen) -- "\n",
    L.

%% @doc 将列表转换成字符串
to_string(AtomList) when is_list(AtomList) ->
    to_string(AtomList,"");
to_string(_) ->
    {error,error_type}.
to_string([], R) -> lists:reverse(R);
to_string([H|T], R) ->
    to_string(T,tuple_to_list(H) ++ R);
to_string(_, _) ->
    {error,error_type}.

%%to_list(L) when is_list(L) ->
%%    to_list(L, []);
%%to_list(_) ->
%%    {error, error_type}.
%%to_list([], R) -> lists:reverse(R);
%%to_list([H|T], R) ->
%%    case is_list(H) of
%%        true ->
%%            to_list(T, [H|R]);
%%        false ->
%%            case is_integer(H) of
%%                true ->
%%                    L4 = integer_to_list(H),
%%                    to_list1(T, [L4|R]);
%%                false ->
%%                    L4 = binary_to_list(H),
%%                    to_list1(T, [L4|R])
%%            end
%%    end.

to_list1(L) when is_list(L) ->
    to_list1(L, []);
to_list1(_) ->
    {error, error_type}.
to_list1([], R) -> lists:reverse(R);
to_list1([H|T], R) ->
    case is_list(H) of
        true ->
            to_list1(T, [H|R]);
        false ->
            case is_integer(H) of
                true ->
                    L4 = integer_to_list(H),
                    to_list1(T, [L4|R]);
                false ->
                    L4 = binary_to_list(H),
                    to_list1(T, [L4|R])
            end
    end.

-spec index(Atom::atom(), List::list()) -> ok.
index(Atom,List) when is_list(List)->
    index(Atom, List, 1);
index(_, _) ->
    error.
index(_,[],_) ->
    -1;
index(Atom, [H|T], Index) ->
    case Atom == H of
        true -> Index;
        false -> index(Atom, T, Index+1)
    end.