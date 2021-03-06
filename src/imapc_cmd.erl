-module(imapc_cmd).

-include("imap.hrl").

-export([send_command/3]).

%%%------------------------
%%% Commands send functions
%%%------------------------

send_command(SockType, Sock, Command) ->
  case prepare_command(Command) of
    {Tag, Line} ->
      case imapc_util:sock_send(SockType, Sock, Line ++ "\r\n") of
        ok ->
          ?LOG_DEBUG("line sent: ~s", [Line]),
          {ok, Tag};
        {error, Reason} ->
          {error, Reason}
      end;
    Line ->
      imapc_util:sock_send(SockType, Sock, Line ++ "\r\n")
  end.

prepare_command({command, login, {User, Pass}}) ->
  prepare_tagged_command("LOGIN ~s ~s", [User, Pass]);
prepare_command({command, logout, {}}) ->
  prepare_tagged_command("LOGOUT", []);
prepare_command({command, list, Args}) ->
  prepare_tagged_command("LIST ~s ~s", Args);
prepare_command({command, status, Args}) ->
  prepare_tagged_command("STATUS ~s ~s", Args);
prepare_command({command, select, Mailbox}) ->
  prepare_tagged_command("SELECT ~s", [Mailbox]);
prepare_command({command, examine, Mailbox}) ->
  prepare_tagged_command("EXAMINE ~s", [Mailbox]);
prepare_command({command, append, [Mailbox, Flags, Message]}) ->
  Tag = imapc_util:gen_tag(),
  Line =Tag ++ " APPEND" ++ [32] ++ Mailbox ++ [32] ++ Flags ++ [32]
        ++ [123] ++ integer_to_list(length(Message)) ++ [125,13,10] ++ Message,
  {Tag, Line};
prepare_command({command, expunge, _Args}) ->
  Tag = imapc_util:gen_tag(),
  {Tag, Tag ++ " EXPUNGE"};
prepare_command({command, search, SearchKeys}) ->
  prepare_tagged_command("SEARCH ~s", [imapc_util:to_key(SearchKeys)]);
prepare_command({command, fetch, Args}) ->
  prepare_tagged_command("FETCH ~s ~s", Args);
prepare_command({command, copy, Args}) ->
  prepare_tagged_command("COPY ~s ~s", Args);
prepare_command({command, store, Args}) ->
  prepare_tagged_command("STORE ~s ~s ~s", Args);
prepare_command({command, noop, {}}) ->
  prepare_tagged_command("NOOP", []).

prepare_tagged_command(Format, Args) ->
  Tag = imapc_util:gen_tag(),
  Line = lists:flatten(io_lib:format("~s " ++ Format, [Tag | Args])),
  {Tag, Line}.

%%%-----------
%%% tests
%%%-----------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

examine_test() ->
    ?assertEqual({"000", "000 EXAMINE [Gmail]/All Mail"},
               prepare_command({command, examine, "[Gmail]/All Mail"})).

-endif.
