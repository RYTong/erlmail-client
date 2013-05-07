-module(imapc).

-include("imap.hrl").

-export([connect/2, connect/3, login/3,
         list/3, status/3, select/2, examine/2, append/4, expunge/1,
         search/2, fetch/3, store/4, copy/3,
         noop/1, logout/1, disconnect/1, quit/1,
         fsm_state/1
        ]).


%%%-----------------
%%% Client functions
%%%-----------------
connect(Host, Port) ->
  connect(Host, Port, []).
connect(Host, Port, Options) ->
  case lists:member(ssl, Options) of 
    true -> gen_fsm:start_link(imapc_fsm, {ssl, Host, Port}, []);
    false -> gen_fsm:start_link(imapc_fsm, {tcp, Host, Port}, [])
  end.

login(Fsm, User, Pass) ->
  gen_fsm:sync_send_event(Fsm, {command, login, {User, Pass}}).

list(Fsm, RefName, Mailbox) ->
  gen_fsm:sync_send_event(Fsm, {command, list, [imapc_util:quote_mbox(RefName), imapc_util:quote_mbox(Mailbox)]}).

%% @link http://tools.ietf.org/html/rfc3501#section-6.3.10
%% The currently defined status data items that can be requested are:
%%   MESSAGES      --->  The number of messages in the mailbox.
%%   RECENT        --->  The number of messages with the \Recent flag set.
%%   UIDNEXT       --->  The next unique identifier value of the mailbox.
%%   UIDVALIDITY   --->  The unique identifier validity value of the mailbox.
%%   UNSEEN        --->  The number of messages which do not have the \Seen flag set.
status(Fsm, Mailbox, StatusDataItems) ->
  gen_fsm:sync_send_event(Fsm, {command, status, [imapc_util:quote_mbox(Mailbox), StatusDataItems]}).

select(Fsm, Mailbox) ->
  {ok, M} = gen_fsm:sync_send_event(Fsm, {command, select, imapc_util:quote_mbox(Mailbox)}),
  {ok, M#mailbox{name = Mailbox}}.

examine(Fsm, Mailbox) ->
  gen_fsm:sync_send_event(Fsm, {command, examine, imapc_util:quote_mbox(Mailbox)}).

%% "From: Fred Foobar <foobar@Blurdybloop.COM>\r\nSubject: afternoon meeting\r\n\r\nddd\r\n"
append(Fsm, Mailbox, Flags, Message) ->
  gen_fsm:sync_send_event(Fsm, {command, append, [imapc_util:quote_mbox(Mailbox), Flags, Message]}).

expunge(Fsm) ->
  gen_fsm:sync_send_event(Fsm, {command, expunge, []}).

search(Fsm, SearchKeys) ->
  gen_fsm:sync_send_event(Fsm, {command, search, SearchKeys}).

fetch(Fsm, SequenceSet, MsgDataItems) ->
  gen_fsm:sync_send_event(Fsm, {command, fetch, [SequenceSet, MsgDataItems]}, infinity).

copy(Fsm, SequenceSet, Mailbox) ->
  gen_fsm:sync_send_event(Fsm, {command, copy, [SequenceSet, imapc_util:quote_mbox(Mailbox)]}).

store(Fsm, SequenceSet, Flags, Action) ->
  gen_fsm:sync_send_event(Fsm, {command, store, [SequenceSet, Flags, Action]}).

logout(Fsm) ->
  gen_fsm:sync_send_event(Fsm, {command, logout, {}}).

disconnect(Fsm) ->
  gen_fsm:sync_send_all_state_event(Fsm, {command, disconnect, {}}).

noop(Fsm) ->
  gen_fsm:sync_send_event(Fsm, {command, noop, {}}).

fsm_state(Fsm) ->
  gen_fsm:sync_send_all_state_event(Fsm, fsm_state).

quit(Fsm) ->
  ok = logout(Fsm),
  ok = disconnect(Fsm).

%%%-------------------
%%% Callback functions
%%%-------------------
% init({ConnType, Host, Port, User, Pass, Options}) ->
%   try
%     ok = ,
%     {ok, Conn}
%   catch
%     error:{badmatch, {error, Reason}} -> {stop, Reason}
%   end.
