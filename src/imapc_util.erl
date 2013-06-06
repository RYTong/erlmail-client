-module(imapc_util).

-include("imap.hrl").
-include("client.hrl").

-export([identity_fun/1, catch_first_error/1, extract_dict_element/2,
         clean_line/1, start_ssl/0, sock_connect/4, sock_send/3, sock_close/2,
         gen_tag/0, quote_mbox/1, to_key/1]).
-export([to_binary/1, to_int/1, to_list/1, to_float/1, to_atom/1]).

-export([parse_fetch_result/1, parse_bodystructure/1]).

-export([mailbox_to_utf8/1, make_envelope/1]).

-compile([nowarn_unused_function]).

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
  quote_mbox_if_need(Orig).

quote_mbox_if_need([$"| Rest]) ->
  [$" | Rest];
quote_mbox_if_need(Orig) ->
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
% parse_fetch_result("INTERNALDATE", Str) ->
%   case re:run(Str, "INTERNALDATE \"(?<DATE>.*?)\"", [{capture, ["DATE"], list}]) of
%     {match, [Date]} -> {ok, Date};
%     _ -> {error, not_found}
%   end;
% parse_fetch_result("RFC822.SIZE", Str) ->
%   ?LOG_DEBUG("parse_fetch_result.RFC822.SIZE: ~p~n", [Str]),
%   case re:run(Str, "RFC822.SIZE (?<SIZE>\\d+)", [{capture, ["SIZE"], list}]) of
%     {match, [Size]} -> {ok, ?l2i(Size)};
%     _ -> {error, not_found}
%   end;
% parse_fetch_result("RFC822", Str) ->
%   case re:run(Str, "\\(RFC822 {\\d+}(?<RAW>.*)\\)", [{capture, ["RAW"], list}, dotall]) of
%     {match, [Raw]} -> {ok, Raw};
%     _ -> {error, not_found}
%   end;
% parse_fetch_result("HAS_ATTACHEMENT", Str) ->
%   ?LOG_DEBUG("parse_fetch_result.HAS_ATTACHEMENT: ~p~n", [Str]),
%   case re:run(Str, "(?i)\\(\"attachment\" \\(\"FILENAME\" \"(?-i)", []) of
%     {match, _} -> true;
%     _ -> false
%   end;
% parse_fetch_result("FLAGS", Str) ->
%   ?LOG_DEBUG("parse_fetch_result.FLAGS: ~p~n", [Str]),
%   case re:run(Str, "FLAGS \\((?<FLAGS>.*?)\\)", [{capture, ["FLAGS"], list}]) of
%     {match, [Flags]} -> {ok, string:tokens(string:to_upper(Flags), " ")};
%     _ -> {error, not_found}
%   end;
% parse_fetch_result("ENVELOPE", Str) ->
%   ?LOG_DEBUG("parse_fetch_result.ENVELOPE: ~p~n", [Str]),
%   RE = "ENVELOPE \\(\"(?<DATE>.*?)\" \"(?<SUBJECT>.*?)\" \\(\\(\"?(?<EMAILNAME>.*?)\"? .*? \"(?<EMAILID>.*?)\" \"(?<EMAILHOST>.*?)\"\\)",
%   Fields = ["DATE","SUBJECT","EMAILNAME","EMAILID","EMAILHOST"],
%   case re:run(Str, RE, [{capture, Fields,list}]) of
%     {match, Value} -> {ok, lists:zip(Fields, Value)};
%     _ -> {error, not_found}
%   end.

parse_fetch_result(Str) ->
  ?LOG_DEBUG("~nparse_fetch_result:~p~n", [Str]),
  parse_fetch_result(Str, []).


parse_fetch_result([], Acc) ->
  pack_kv(hd(Acc));
parse_fetch_result([$( | Rest], Acc) ->
  {Str2, Acc2} = parse_fetch_result(Rest, []),
  parse_fetch_result(Str2, Acc ++ [Acc2]);
parse_fetch_result(Str = [$" | _], Acc) ->
  {match, [{0, Len}]} = re:run(Str, "\".*?\""),
  {Value, Str2} = lists:split(Len-1, Str), 
  parse_fetch_result(tl(Str2), Acc ++ [tl(Value)]);
parse_fetch_result(Str = [${ | _], Acc) ->
  {match, [{0, Len}]} = re:run(Str, "{\\d+}"),
  {Value, Str2} = lists:split(Len, Str), 
  N = list_to_integer(lists:sublist(Value, 2, Len-2)), 
  {Value2, Str3} = lists:split(N, Str2), 
  parse_fetch_result(Str3, Acc ++ [Value2]);

parse_fetch_result([$\r, $\n | Rest], Acc) ->
  parse_fetch_result(Rest, Acc);
parse_fetch_result([32 | Rest], Acc) ->
  parse_fetch_result(Rest, Acc);
parse_fetch_result([$) | Rest], Acc) ->
  {Rest, Acc};
parse_fetch_result(Str, Acc) ->
  {match, [{0, Len}]} = re:run(Str, "(.*?)[\\s)]", [{capture, all_but_first}]),
  {Value, Str2} = lists:split(Len, Str), 
  Value2 =
    case is_integer_str(Value) of
      true -> list_to_integer(Value); 
      false -> list_to_atom(Value) 
    end,
  parse_fetch_result(Str2, Acc ++ [Value2]).

make_envelope([DATE, SUBJECT, FROMs, SENDERs, REPLYTOs, TOs, CCs, BCCs, INREPLYTO, MESSAGEID]) ->
  #envelope{
    env_date = DATE,
    env_subject = header_to_utf8(SUBJECT),
    env_from = make_addresses(FROMs),
    env_sender = make_addresses(SENDERs),
    env_reply_to = make_addresses(REPLYTOs),
    env_to = make_addresses(TOs),
    env_cc = make_addresses(CCs),
    env_bcc = make_addresses(BCCs),
    env_in_reply_to = header_to_utf8(INREPLYTO),
    env_message_id = MESSAGEID
  }.
make_addresses('NIL') -> 'NIL';
make_addresses(Addrs) -> make_addresses(Addrs, []).
make_addresses([], Acc) -> Acc;
make_addresses([[NAME, ADL, MAILBOX, HOST]| Rest], Acc) ->
  Addr = #address{
    addr_name = header_to_utf8(NAME),
    addr_adl = ADL,
    addr_mailbox = MAILBOX,
    addr_host = HOST
  },
  make_addresses(Rest, [Addr | Acc]).

% body            = "(" (body-type-1part / body-type-mpart) ")"

% body-type-1part = (body-type-basic / body-type-msg / body-type-text)
%                   [SP body-ext-1part]
% body-type-basic = media-basic SP body-fields
% media-basic     = ((DQUOTE ("APPLICATION" / "AUDIO" / "IMAGE" /
%                   "MESSAGE" / "VIDEO") DQUOTE) / string) SP
%                   media-subtype
% media-subtype   = string
% body-fields     = body-fld-param SP body-fld-id SP body-fld-desc SP
%                   body-fld-enc SP body-fld-octets
% body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil
% body-fld-id     = nstring
% body-fld-desc   = nstring
% body-fld-enc    = (DQUOTE ("7BIT" / "8BIT" / "BINARY" / "BASE64"/
%                   "QUOTED-PRINTABLE") DQUOTE) / string
% body-fld-octets = number

% body-type-msg   = media-message SP body-fields SP envelope
%                   SP body SP body-fld-lines
% media-message   = DQUOTE "MESSAGE" DQUOTE SP DQUOTE "RFC822" DQUOTE

% body-type-text  = media-text SP body-fields SP body-fld-lines
% media-text      = DQUOTE "TEXT" DQUOTE SP media-subtype
% body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
%                   [SP body-fld-loc *(SP body-extension)]]]


% body-type-mpart = 1*body SP media-subtype
%                   [SP body-ext-mpart]
% media-subtype   = string
% body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
%                   [SP body-fld-loc *(SP body-extension)]]]
% body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil
% body-fld-dsp    = "(" string SP body-fld-param ")" / nil
% body-fld-lang   = nstring / "(" string *(SP string) ")"
% body-fld-loc    = nstring
% body-extension  = nstring / number / "(" body-extension *(SP body-extension) ")"
% nstring         = string / nil
% string          = quoted / literal
% quoted          = DQUOTE *QUOTED-CHAR DQUOTE
% QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> / "\" quoted-specials
% quoted-specials = DQUOTE / "\"
% literal         = "{" number "}" CRLF *CHAR8

% body            = "(" (body-type-1part / body-type-mpart) ")"
parse_bodystructure(BodyStructure) ->
  S = parse_bodystructure(BodyStructure, [1]),
  case parse_bodystructure_1([S]) of
    [{_, SubType, Parts}] -> {"BODY[]", SubType, Parts};
    [Part] -> Part
  end.
% body-type-1part = (body-type-basic / body-type-msg / body-type-text) [SP body-ext-1part]
% body-type-msg   = media-message SP body-fields SP envelope SP body SP body-fld-lines
parse_bodystructure(["message", "rfc822", Params, Id, Desc, Enc, Octets, _, _, Lines], P) ->
  body_type_msg(P, Params, Id, Desc, Enc, Octets, Lines);
parse_bodystructure(["message", "rfc822", Params, Id, Desc, Enc, Octets, _, _, Lines, _Md5, Dsp|_], P) ->
  body_type_msg(P, Params, Id, Desc, Enc, Octets, Lines, Dsp);
parse_bodystructure(["message", "rfc822", Params, Id, Desc, Enc, Octets, _, _, Lines|_], P) ->
  body_type_msg(P, Params, Id, Desc, Enc, Octets, Lines);
% body-type-text  = media-text SP body-fields SP body-fld-lines
parse_bodystructure(["text", SubType, Params, Id, Desc, Enc, Octets, Lines], P) ->
  body_type_text(P, SubType, Params, Id, Desc, Enc, Octets, Lines);
parse_bodystructure(["text", SubType, Params, Id, Desc, Enc, Octets, Lines, _Md5, Dsp|_], P) ->
  body_type_text(P, SubType, Params, Id, Desc, Enc, Octets, Lines, Dsp);
parse_bodystructure(["text", SubType, Params, Id, Desc, Enc, Octets, Lines|_], P) ->
  body_type_text(P, SubType, Params, Id, Desc, Enc, Octets, Lines);
% body-type-basic = media-basic SP body-fields
parse_bodystructure([Type, SubType, Params, Id, Desc, Enc, Octets], P) when is_list(Type), is_integer(hd(Type))->
  body_type_basic(P, Type, SubType, Params, Id, Desc, Enc, Octets);
parse_bodystructure([Type, SubType, Params, Id, Desc, Enc, Octets, _Md5, Dsp|_], P) when is_list(Type), is_integer(hd(Type))->
  body_type_basic(P, Type, SubType, Params, Id, Desc, Enc, Octets, Dsp);
parse_bodystructure([Type, SubType, Params, Id, Desc, Enc, Octets|_], P) when is_list(Type), is_integer(hd(Type))->
  body_type_basic(P, Type, SubType, Params, Id, Desc, Enc, Octets);
%% body-type-mpart = 1*body SP media-subtype [SP body-ext-mpart]
parse_bodystructure(MPart, P) ->
  SubTypePos = find_mpart_subtype_pos(MPart),
  {Bodies, [SubType|_]} = lists:split(SubTypePos, MPart), 
  {_, Parts} = lists:foldl(
    fun(Body, {N, Acc}) ->
      {N+1, [parse_bodystructure(Body, [N | P]) | Acc]}
    end, {1, []}, Bodies), 
  {P, lists:concat(["multipart/", SubType]), Parts}.

%% add body specifier.
parse_bodystructure_1(Parts) ->
  parse_bodystructure_1(Parts, []).
parse_bodystructure_1([], Acc) ->
  Acc;
parse_bodystructure_1([{P, SubType, Parts} | Rest], Acc) ->
  Acc2 = parse_bodystructure_1(Parts),
  parse_bodystructure_1(Rest, [{body_specifier(P), SubType, Acc2} | Acc]);
parse_bodystructure_1([{P, Body} | Rest], Acc) ->
  parse_bodystructure_1(Rest, [{body_specifier(P), Body} | Acc]).



%% @doc Decode and Encode rfc3501 Mailbox.
mailbox_to_utf8(Str) when is_list(Str) ->
  mailbox_to_utf8(Str, [], []).
mailbox_to_utf8([], [], Acc) ->
  lists:flatten(lists:reverse(Acc));
mailbox_to_utf8([], Utf7Acc, Acc) ->
  mailbox_to_utf8([], [], [decode_rfc3501_mailbox(Utf7Acc) | Acc]);
mailbox_to_utf8([$& | Rest], [], Acc) ->
  mailbox_to_utf8(Rest, [$&], Acc);
mailbox_to_utf8([$- | Rest], [], Acc) ->
  mailbox_to_utf8(Rest, [], Acc);
mailbox_to_utf8([$- | Rest], [_|[]], Acc) ->
  mailbox_to_utf8(Rest, [], [$& | Acc]);
mailbox_to_utf8([$- | Rest], Utf7Acc, Acc) ->
  mailbox_to_utf8(Rest, [], [decode_rfc3501_mailbox(Utf7Acc) | Acc]);
mailbox_to_utf8([C | Rest], [], Acc) ->
  mailbox_to_utf8(Rest, [], [C | Acc]);
mailbox_to_utf8([C | Rest], Utf7Acc, Acc) ->
  mailbox_to_utf8(Rest, [C | Utf7Acc], Acc).

utf8_to_mailbox(Str) when is_list(Str) ->
  utf8_to_mailbox(Str, [], []).
utf8_to_mailbox([], [], Acc) ->
  lists:flatten(lists:reverse(Acc)); 
utf8_to_mailbox([], Utf7Acc, Acc) ->
  utf8_to_mailbox([], [], [encode_rfc3501_mailbox(Utf7Acc) | Acc]);
utf8_to_mailbox([C | Rest], Utf7Acc, Acc) when C >= 16#20, C < 16#26 ->
  utf8_to_mailbox(Rest, [], [C, encode_rfc3501_mailbox(Utf7Acc) | Acc]);
utf8_to_mailbox([C | Rest], Utf7Acc, Acc) when C >= 16#27, C<16#7F ->
  utf8_to_mailbox(Rest, [], [C, encode_rfc3501_mailbox(Utf7Acc) | Acc]);
utf8_to_mailbox([$& | Rest], Utf7Acc, Acc) ->
  utf8_to_mailbox(Rest, [], [$-, $&, encode_rfc3501_mailbox(Utf7Acc) | Acc]);
utf8_to_mailbox([C | Rest], Utf7Acc, Acc) ->
  utf8_to_mailbox(Rest, [C | Utf7Acc], Acc).



%%%-----------
%%% internal
%%%-----------
%% XXX:Seems there is a bug in converting to utf7 from utf8 using erlang-iconv.
encode_rfc3501_mailbox([]) -> [];
encode_rfc3501_mailbox(S) ->
  {ok, Cd} = iconv:open("UTF7", "UTF8"),
  {ok, Bin} = iconv:conv(Cd, lists:reverse(S)),
  iconv:close(Cd), 
  [$&, tl(re:replace(Bin, "/", ",", [{return, list}]))] ++ [$-].

decode_rfc3501_mailbox(S) ->
  [_ | S2] = lists:reverse(S), 
  S_utf7 = re:replace(S2, ",", "/", [{return, binary}]),
  {ok, Cd} = iconv:open("UTF8", "UTF7"),
  {ok, Bin} = iconv:conv(Cd, <<$+, S_utf7/binary, $->>),
  iconv:close(Cd), 
  [?b2l(Bin)].

is_integer_str([]) -> false;
is_integer_str(List) when is_list(List) -> is_integer_str_1(List); 
is_integer_str(_) -> false.
is_integer_str_1([]) -> true;
is_integer_str_1([H | Rest]) when is_integer(H),H >= 48,H =< 57 -> is_integer_str_1(Rest); 
is_integer_str_1(_) -> false.

header_to_utf8(Content) when is_list(Content) ->
  header_to_utf8(list_to_binary(Content));
header_to_utf8(Content) when is_binary(Content) ->
  try
    binary_to_list(mimemail:decode_header(Content, "utf8"))
   catch _:_ ->
     ?LOG_DEBUG("failed to decode header: ~p~n", [Content]),
     binary_to_list(Content) 
   end;
header_to_utf8(Other) ->
  Other.

find_mpart_subtype_pos(MPart) ->
  find_mpart_subtype_pos(MPart, 0).
find_mpart_subtype_pos([SubType | _Rest], Pos) when is_list(SubType), is_integer(hd(SubType))->
  Pos;
find_mpart_subtype_pos([_ | Rest], Pos) ->
  find_mpart_subtype_pos(Rest, Pos+1).

pack_kv('NIL') -> [];
pack_kv(Params) ->
  lists:foldl(
    fun(Value, {Key, Acc2}) -> [{Key, Value} | Acc2]; 
       (Key, Acc2) -> {Key, Acc2}
    end, [], Params). 

body_specifier([N]) ->
  lists:concat(["BODY[", integer_to_list(N), "]"]); 
body_specifier(NList) ->
  Nums = tl(lists:reverse(NList)),
  Nums2 = tl(lists:concat(["."++integer_to_list(N) || N <- Nums])),
  lists:concat(["BODY[", Nums2, "]"]).

field_dsp('NIL') ->
  #disposition{};
field_dsp([Type, Value]) ->
  #disposition{
    type = Type,
    value = pack_kv(Value)
  }.

body_type_msg(P, Params, Id, Desc, Enc, Octets, Lines) ->
  body_type_msg(P, Params, Id, Desc, Enc, Octets, Lines, [undefined, []]).
body_type_msg(P, Params, Id, Desc, Enc, Octets, Lines, Dsp) ->
  {P, #body_type_other{
        basic = #body_type_basic{
          type = "message/rfc822",
          fields = #body_fields{
                          param = pack_kv(Params),
                          id = Id,
                          desc = Desc,
                          enc = Enc,
                          octets = Octets,
                          dsp = field_dsp(Dsp)}},
        lines = Lines}
  }.

body_type_text(P, SubType, Params, Id, Desc, Enc, Octets, Lines) ->
  body_type_text(P, SubType, Params, Id, Desc, Enc, Octets, Lines, [undefined, []]).
body_type_text(P, SubType, Params, Id, Desc, Enc, Octets, Lines, Dsp) ->
  {P, #body_type_other{
        basic = #body_type_basic{
          type = lists:concat(["text/", SubType]), 
          fields = #body_fields{
                          param = pack_kv(Params),
                          id = Id,
                          desc = Desc,
                          enc = Enc,
                          octets = Octets,
                          dsp = field_dsp(Dsp)}},
        lines = Lines}
  }.

body_type_basic(P, Type, SubType, Params, Id, Desc, Enc, Octets) ->
  body_type_basic(P, Type, SubType, Params, Id, Desc, Enc, Octets, [undefined, []]).
body_type_basic(P, Type, SubType, Params, Id, Desc, Enc, Octets, Dsp) ->
  {P, #body_type_basic{
        type = lists:concat([Type, "/", SubType]),
        fields = #body_fields{
                      param = pack_kv(Params),
                      id = Id,
                      desc = Desc,
                      enc = Enc,
                      octets = Octets,
                      dsp = field_dsp(Dsp)}}
  }.

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