-module(imapc_resp).

-include("imap.hrl").

-export([parse_response/1, analyze_response/4]).

%%%----------------------------
%%% Responses parsing functions
%%%----------------------------

parse_response(Line) ->
    case string:tokens(Line, " ") of
        [] ->
            {ok, {response, untagged, [], []}};
        [_] ->
            {ok, {response, untagged, Line, []}};
        _ ->
            Resp = string:to_upper(lists:nth(2, string:tokens(Line, " "))),
            io:format("resp====:~p~n", [Resp]), 
            case int_parse_response(Resp, Line) of
                {match, Response} -> {ok, parse_tag(Response)};
                nomatch -> {ok, {response, untagged, Line, []}}
            end
    end.

%% TODO TODO TODO
%% LOGIN
analyze_response(not_authenticated, Responses, {command, login, {_, _}}, From) ->
  case get_response_result(Responses) of
    {result, ok} ->
      send_client_response_result(ok, From),
      authenticated;
    {result, Other} ->
      send_client_response_result(Other, From),
      not_authenticated
  end;
%% LOGOUT
analyze_response(StateName, Responses, {command, logout, {}}, From) ->
  HasBye = check_response_has(Responses, "BYE"),
  {result, Result} = get_response_result(Responses),
  case {HasBye, Result} of
    {true, ok} ->
      send_client_response_result(ok, From),
      logout;
    {_, Result} ->
      send_client_response_result(Result, From),
      StateName
  end;

%% LIST  - TODO: rfc stipulations
analyze_response(authenticated, Responses, {command, list, _}, From) ->
  % Responses = [Response],
  % Response = {response,untagged,"LIST",{"[RYTong]",["\\HasChildren"]}}
  send_client_response_result({ok, list_value(Responses)}, From),
  authenticated;

%% STATUS  - TODO: rfc stipulations
analyze_response(authenticated, Responses, {command, status, _}, From) ->
  send_client_response_result({ok, status_value(Responses)}, From),
  authenticated;

%% SELECT  - TODO: rfc stipulations
analyze_response(authenticated, Responses, {command, select, _}, From) ->
  send_client_response_result({ok, select_value(Responses)}, From),
  authenticated;

%% EXAMINE  - TODO: rfc stipulations
analyze_response(authenticated, Responses, {command, examine, _}, From) ->
  send_client_response_result({ok, Responses}, From),
  authenticated;
  
%% SEARCH   - TODO: rfc stipulations
analyze_response(authenticated, Responses, {command, search, _}, From) ->
  send_client_response_result({ok, Responses}, From),
  authenticated;

%% FETCH   - TODO: rfc stipulations
analyze_response(authenticated, Responses, {command, fetch, _}, From) ->
  Value = fetch_value(Responses),
  Value2 = [{MsgSeq, lists:flatten(lists:reverse(X))} || {MsgSeq, X} <- Value],
  send_client_response_result({ok, Value2}, From),
  authenticated;

%% STORE   - TODO: rfc stipulations
analyze_response(authenticated, Responses, {command, store, _}, From) ->
  send_client_response_result({ok, Responses}, From),
  authenticated;

%% NOOP
analyze_response(StateName, Responses, {command, noop, {}}, From) ->
  {result, Result} = get_response_result(Responses),
  send_client_response_result(Result, From),
  StateName;

%% Ignore other resp.
analyze_response(StateName, Responses, Command, From) ->
  ?LOG_DEBUG("ignore resp of cmd#~p~n", [Command]),
  ?LOG_DEBUG("Resp: ~p~n", [Responses]),
  send_client_response_result({ok, i_dont_care_the_resp}, From),
  StateName.
  
%%%-----------
%%% internal
%%%-----------

parse_tag({response, "+", Response, Args}) ->
  {response, tag_continue, Response, Args};
parse_tag({response, "*", Response, Args}) ->
  {response, untagged, Response, Args};
parse_tag({response, _Tag, _, _} = ResponseTuple) ->
  ResponseTuple.

%% TODO TODO TODO

send_client_response_result(ok, From) ->
  gen_fsm:reply(From, ok);
send_client_response_result({ok, Reply}, From) ->
  gen_fsm:reply(From, {ok, Reply});
send_client_response_result(no, From) ->
  gen_fsm:reply(From, {error, no_response});
send_client_response_result(bad, From) ->
  gen_fsm:reply(From, {error, bad_response}).

get_response_result(Responses) ->
  case lists:last(Responses) of
    {response, Tag, "OK", _} when not is_atom(Tag) -> {result, ok};
    {response, Tag, "NO", _} when not is_atom(Tag) -> {result, no};
    {response, Tag, "BAD", _} when not is_atom(Tag) -> {result, bad}
  end.

check_response_has(Responses, What) ->
  case lists:keysearch(What, 3, Responses) of
    {value, _} -> true;
    false -> false
  end.

int_parse_response("CAPABILITY", Line) ->
  imapc_re:match_capability_response(Line);
int_parse_response("OK", Line) ->
  imapc_re:match_ok_response(Line);
int_parse_response("NO", Line) ->
  imapc_re:match_no_response(Line);
int_parse_response("BAD", Line) ->
  imapc_re:match_bad_response(Line);
int_parse_response("BYE", Line) ->
  imapc_re:match_bye_response(Line);
int_parse_response("FLAGS", Line) ->
  imapc_re:match_flags_response(Line);
int_parse_response("SEARCH", Line) ->
  imapc_re:match_search_response(Line);
int_parse_response("LIST", Line) ->
  imapc_re:match_list_response(Line);
int_parse_response("STATUS", Line) ->
  imapc_re:match_status_response(Line);
int_parse_response(_Resp, Line) ->
  try
    Third = string:to_upper(lists:nth(3, string:tokens(Line, " \r\n"))),
    io:format("third:===== ~p~n", [Third]), 
    case try_third_term(Third, Line) of
      {match, Response} ->
        {match, Response};
      _ ->
        %%?LOG_ERROR(int_parse_response, "Unhandled response: ~p~n~p~n", [Resp,Line]),
        nomatch
    end
  catch
    _:_Err ->
      %%?LOG_DEBUG("try_third_term error: ~p", [Err]),
      nomatch
  end.

try_third_term("EXISTS", Line) ->
  imapc_re:match_exists_response(Line);
try_third_term("RECENT", Line) ->
  imapc_re:match_recent_response(Line);
try_third_term("FETCH", Line) ->
  imapc_re:match_fetch_response(Line);
try_third_term(_,_) ->
  nomatch.

%% convert `LIST` responses to proplists.
list_value(Responses) ->
  ?LOG_DEBUG("list resp: ~p~n", [Responses]),
  list_value(Responses, []).
list_value([], Acc) -> Acc;
list_value([{response,untagged,"LIST",{Name, Attrs}} | Rest], Acc) ->
  list_value(Rest, [{Name, Attrs} | Acc]);
list_value([_ | Rest], Acc) ->
  list_value(Rest, Acc).

%% convert `STATUS` responses to proplists.
status_value(Responses) ->
  ?LOG_DEBUG("status resp: ~p~n", [Responses]),
  status_value(Responses, []).
status_value([], Acc) -> Acc;
status_value([{response,untagged,"STATUS", {Mailbox, Items}} | Rest], Acc) ->
  status_value(Rest, [{Mailbox, status_value_1(Items, [])} | Acc]);
status_value([_ | Rest], Acc) ->
  status_value(Rest, Acc).

status_value_1([], Acc) -> Acc;
status_value_1([K, V|Rest], Acc) -> status_value_1(Rest, [{K, V}|Acc]).

%% convert `SELECT` responses to imap mailbox record.
select_value(Responses) ->
  ?LOG_DEBUG("selece resp: ~p~n", [Responses]),
  select_value(Responses, #mailbox{}).
select_value([], Mailbox) -> Mailbox; 
select_value([{response,untagged,"EXISTS", Value} | Rest], Mailbox) ->
  select_value(Rest, Mailbox#mailbox{exists = ?l2i(Value)});
select_value([{response,untagged,"RECENT", Value} | Rest], Mailbox) ->
  select_value(Rest, Mailbox#mailbox{recent = ?l2i(Value)});
select_value([{response,untagged,"FLAGS", Value} | Rest], Mailbox) ->
  select_value(Rest, Mailbox#mailbox{flags = string:tokens(Value, "( )")});
select_value([{response,untagged,"OK", {[Key, Value], _}} | Rest], Mailbox) ->
  case Key of
    "UNSEEN" -> select_value(Rest, Mailbox#mailbox{unseen = ?l2i(Value)});
    "UIDVALIDITY" -> select_value(Rest, Mailbox#mailbox{uidvalidity = ?l2i(Value)});
    "UIDNEXT" -> select_value(Rest, Mailbox#mailbox{uidnext = ?l2i(Value)});
    "HIGHESTMODSEQ" -> select_value(Rest, Mailbox#mailbox{highestmodseq = ?l2i(Value)});
    _ -> select_value(Rest, Mailbox)
  end;
select_value([{response,untagged,"OK", {["PERMANENTFLAGS"|RestValue], _}} | Rest], Mailbox) ->
  select_value(Rest, Mailbox#mailbox{permflags = string:tokens(string:join(RestValue," ") , "( )")});
select_value([{response,_,"OK", {["READ-WRITE"], _}} | Rest], Mailbox) ->
  select_value(Rest, Mailbox#mailbox{readwrite=true});
select_value([R | Rest], Mailbox) ->
  ?LOG_DEBUG("ignore select response: ~p~n", [R]), 
  select_value(Rest, Mailbox).

%% merge all `FETCH` responses.
fetch_value(Responses) ->
  ?LOG_DEBUG("fetch resp: ~p~n", [Responses]),
  fetch_value(Responses, []).
fetch_value([], Acc) -> Acc;
fetch_value([{response,untagged,"FETCH", {MsgSeq, Value}}|Rest], Acc) ->
  fetch_value(Rest, [{?l2i(MsgSeq), [Value]} | Acc]);
fetch_value([{response,untagged,[],[]}|Rest], [{K,V}|Acc]) ->
  fetch_value(Rest, [{K, [" "| V]} | Acc]);
fetch_value([{response,untagged,Value, []}|Rest], [{K,V}|Acc]) when is_list(Value)->
  fetch_value(Rest, [{K, [Value | V]} | Acc]);
fetch_value([R | Rest], Acc) ->
  ?LOG_DEBUG("ignore fetch response: ~p~n", [R]),
  fetch_value(Rest, Acc).



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_response_test() ->
  ?assertEqual({ok, {response, untagged, "OK", {[],[]}}},
               parse_response("* OK")),
  ?assertEqual({ok, {response, "a01", "OK",
                     {["capability", "IMAP4rev1", "foo"], "Hey you."}}},
               parse_response("a01 ok [capability IMAP4rev1 foo] Hey you.")),
  ?assertEqual({ok, {response, "1234", "NO", "bad boy"}},
               parse_response("1234 no bad boy")),
  ?assertEqual({ok, {response, untagged, "NO", ""}},
               parse_response("* NO")),
  ?assertEqual({ok, {response, "XyZ", "BAD", ""}},
               parse_response("XyZ bad")),
  ?assertEqual({ok, {response, untagged, "BAD", "go to hell"}},
               parse_response("* BAD go to hell")),
  ?assertEqual({ok, {response, untagged, "BYE", ""}},
               parse_response("* BYE see you soon")),
  ?assertEqual({ok, {response, untagged, "CAPABILITY",
                     ["IMAP4rev1","UNSELECT","IDLE","NAMESPACE","QUOTA","ID",
                      "XLIST","CHILDREN","X-GM-EXT-1","UIDPLUS",
                      "COMPRESS=DEFLATE"]}},
     parse_response("* CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID "
                   "XLIST CHILDREN X-GM-EXT-1 UIDPLUS COMPRESS=DEFLATE\r\n")),
  ?assertEqual({ok, {response, untagged, "Delivered-To: dude@gmail.com",
                    []}},
                parse_response("Delivered-To: dude@gmail.com")),
  %?assertEqual({error, nomatch}, parse_response("01 BYE")),
  ok.

int_parse_response_test() ->
  ?assertEqual({match, {response, "*", "EXISTS", "28"}},
               int_parse_response("28", "* 28 EXISTS")),
  ok.

-endif.