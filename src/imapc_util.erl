-module(imapc_util).

-include("imap.hrl").

-export([identity_fun/1, catch_first_error/1, extract_dict_element/2,
         clean_line/1, start_ssl/0, sock_connect/4, sock_send/3, sock_close/2,
         gen_tag/0, quote_mbox/1, to_key/1]).
-export([to_binary/1, to_int/1, to_list/1, to_float/1, to_atom/1]).

-export([parse_fetch_result/2]).

-export([mailbox_to_utf8/1, utf8_to_mailbox/1]).

%%%------------------
%%% Utility functions
%%%------------------

identity_fun(X) -> X.

catch_first_error(Fun) ->
    try Fun()
    catch
      error:{badmatch, {error, Reason}} -> {error, Reason}
    end.

extract_dict_element(Key, Dict) ->
  {ok, Val} = dict:find(Key, Dict),
  {ok, Val, dict:erase(Key, Dict)}.

clean_line(Line) ->
  Line2 = string:strip(Line, right, $\n),
  string:strip(Line2, right, $\r).

start_ssl() ->
  case ssl:start() of
    ok -> ok;
    {error, {already_started, ssl}} -> ok
  end.

sock_connect(tcp, Host, Port, Opts) ->
  gen_tcp:connect(Host, Port, Opts);
sock_connect(ssl, Host, Port, Opts) ->
  ok = start_ssl(),
  ssl:connect(Host, Port, Opts).

sock_send(tcp, Sock, Data) ->
  gen_tcp:send(Sock, Data);
sock_send(ssl, Sock, Data) ->
  ssl:send(Sock, to_binary(Data)).

sock_close(tcp, Sock) ->
  gen_tcp:close(Sock);
sock_close(ssl, Sock) ->
  ssl:close(Sock).

gen_tag() ->
  Tag = case get(last_tag) of
          undefined -> 0;
          LastTag -> LastTag + 1
	end,
  put(last_tag, Tag),
  string:right(integer_to_list(Tag), 3, $0).

quote_mbox(Orig) ->
  lists:flatten([34,Orig,34]).

to_key(L) when is_list(L) ->
  to_key(L, []).

to_key([], Acc) -> Acc;
to_key([H|T], Acc) ->
  Key = string:to_upper(to_list(H)),
  to_key(T, [Key | Acc]).

to_binary(undefined)            -> undefined;
to_binary(V) when is_integer(V) -> to_binary(?i2l(V));
to_binary(V) when is_list(V)    -> to_binary(?l2b(V));
to_binary(V) when is_float(V)   -> to_binary(float_to_list(V));
to_binary(V) when is_binary(V)  -> V.

to_int(undefined)            -> undefined;
to_int(V) when is_float(V)   -> round(V);
to_int(V) when is_integer(V) -> V;
to_int(V) when is_list(V)    -> ?l2i(V);
to_int(V) when is_binary(V)  -> to_int(?b2l(V)).

to_list(undefined)            -> undefined;
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_list(V)    -> V;
to_list(V) when is_binary(V)  -> ?b2l(V);
to_list(V) when is_atom(V)    -> ?a2l(V).

to_float(undefined)            -> undefined;
to_float(V) when is_integer(V) -> V + 0.0;
to_float(V) when is_list(V)    -> list_to_float(V);
to_float(V) when is_binary(V)  -> to_float(?b2l(V)).

to_atom(undefined)         -> undefined;
to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V)                 -> to_atom(to_list(V)).

%% FIXME: We SHOULD NOT parse `FETCH` result using regex.
parse_fetch_result("INTERNALDATE", Str) ->
  case re:run(Str, "INTERNALDATE \"(?<DATE>.*?)\"", [{capture, ["DATE"], list}]) of
    {match, [Date]} -> {ok, Date};
    _ -> {error, not_found}
  end;
parse_fetch_result("RFC822.SIZE", Str) ->
  ?LOG_DEBUG("parse_fetch_result.RFC822.SIZE: ~p~n", [Str]),
  case re:run(Str, "RFC822.SIZE (?<SIZE>\\d+)", [{capture, ["SIZE"], list}]) of
    {match, [Size]} -> {ok, ?l2i(Size)};
    _ -> {error, not_found}
  end;
parse_fetch_result("RFC822", Str) ->
  case re:run(Str, "\\(RFC822 {\\d+}(?<RAW>.*)\\)", [{capture, ["RAW"], list}, dotall]) of
    {match, [Raw]} -> {ok, Raw};
    _ -> {error, not_found}
  end;
parse_fetch_result("HAS_ATTACHEMENT", Str) ->
  ?LOG_DEBUG("parse_fetch_result.HAS_ATTACHEMENT: ~p~n", [Str]),
  case re:run(Str, "\\(\"attachment\" \\(\"FILENAME\" \"", []) of
    {match, _} -> true;
    _ -> false
  end;
parse_fetch_result("FLAGS", Str) ->
  ?LOG_DEBUG("parse_fetch_result.FLAGS: ~p~n", [Str]),
  case re:run(Str, "FLAGS \\((?<FLAGS>.*?)\\)", [{capture, ["FLAGS"], list}]) of
    {match, [Flags]} -> {ok, string:tokens(string:to_upper(Flags), " ")};
    _ -> {error, not_found}
  end;
parse_fetch_result("ENVELOPE", Str) ->
  ?LOG_DEBUG("parse_fetch_result.ENVELOPE: ~p~n", [Str]),
  RE = "ENVELOPE \\(\"(?<DATE>.*?)\" \"(?<SUBJECT>.*?)\" \\(\\(\"?(?<EMAILNAME>.*?)\"? .*? \"(?<EMAILID>.*?)\" \"(?<EMAILHOST>.*?)\"\\)",
  Fields = ["DATE","SUBJECT","EMAILNAME","EMAILID","EMAILHOST"],
  case re:run(Str, RE, [{capture, Fields,list}]) of
    {match, Value} -> {ok, lists:zip(Fields, Value)};
    _ -> {error, not_found}
  end.


%% @doc Decode mailbox name to utf8 data according to
%%      rfc3501 Mailbox International Naming Convention.
mailbox_to_utf8(Str) when is_list(Str) ->
  mailbox_to_utf8(Str, [], []).
mailbox_to_utf8([], [], Acc) ->
  lists:flatten(lists:reverse(Acc));
mailbox_to_utf8([], Utf7Acc, Acc) ->
  mailbox_to_utf8([], [], [decode_rfc3501_mb(Utf7Acc) | Acc]);
mailbox_to_utf8([$& | Rest], [], Acc) ->
  mailbox_to_utf8(Rest, [$&], Acc);
mailbox_to_utf8([$- | Rest], [], Acc) ->
  mailbox_to_utf8(Rest, [], Acc);
mailbox_to_utf8([$- | Rest], [_|[]], Acc) ->
  mailbox_to_utf8(Rest, [], [$& | Acc]);
mailbox_to_utf8([$- | Rest], Utf7Acc, Acc) ->
  mailbox_to_utf8(Rest, [], [decode_rfc3501_mb(Utf7Acc) | Acc]);
mailbox_to_utf8([C | Rest], [], Acc) ->
  mailbox_to_utf8(Rest, [], [C | Acc]);
mailbox_to_utf8([C | Rest], Utf7Acc, Acc) ->
  mailbox_to_utf8(Rest, [C | Utf7Acc], Acc).

%% @doc Encode utf8 data to mailbox name according to
%%      rfc3501 Mailbox International Naming Convention.
utf8_to_mailbox(Str) when is_list(Str) ->
  utf8_to_mailbox(Str, [], []).
utf8_to_mailbox([], [], Acc) ->
  lists:flatten(lists:reverse(Acc)); 
utf8_to_mailbox([], Utf7Acc, Acc) ->
  utf8_to_mailbox([], [], [encode_rfc3501mb(Utf7Acc) | Acc]);
utf8_to_mailbox([C | Rest], Utf7Acc, Acc) when C >= 16#20, C < 16#26 ->
  utf8_to_mailbox(Rest, [], [C, encode_rfc3501mb(Utf7Acc) | Acc]);
utf8_to_mailbox([C | Rest], Utf7Acc, Acc) when C >= 16#27, C<16#7F ->
  utf8_to_mailbox(Rest, [], [C, encode_rfc3501mb(Utf7Acc) | Acc]);
utf8_to_mailbox([$& | Rest], Utf7Acc, Acc) ->
  utf8_to_mailbox(Rest, [], [$-, $&, encode_rfc3501mb(Utf7Acc) | Acc]);
utf8_to_mailbox([C | Rest], Utf7Acc, Acc) ->
  utf8_to_mailbox(Rest, [C | Utf7Acc], Acc).



%%%-----------
%%% internal
%%%-----------
encode_rfc3501mb([]) -> [];
encode_rfc3501mb(S) ->
  {ok, Cd} = iconv:open("UTF7", "UTF8"),
  {ok, Bin} = iconv:conv(Cd, lists:reverse(S)),
  [$&, re:replace(Bin, "/", ",", [{return, list}])] ++ [$-].

decode_rfc3501_mb(S) ->
  [_ | S2] = lists:reverse(S), 
  S_utf7 = re:replace(S2, ",", "/", [{return, binary}]),
  {ok, Cd} = iconv:open("UTF8", "UTF7"),
  {ok, Bin} = iconv:conv(Cd, <<$+, S_utf7/binary, $->>),
  [?b2l(Bin)].


%%%-----------
%%% tests
%%%-----------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

catch_first_error_test() ->
    {error, foobar} = catch_first_error(fun force_badmatch/0).

force_badmatch() ->
  ok = ok,
  ok = return_error(foobar),
  ok = ok.

return_error(Reason) ->
  {error, Reason}.

-endif.