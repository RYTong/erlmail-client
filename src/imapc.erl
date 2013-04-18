-module(imapc).

-include("imap.hrl").

-behaviour(gen_server).

-export([open_account/5, close_account/1,
         list/3, status/3, select/2, examine/2, append/4, expunge/1,
         search/2, fetch/3, store/4, copy/3
        ]).

-export([fsm_state/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

%%%-----------------
%%% Client functions
%%%-----------------

open_account(ConnType, Host, Port, User, Pass) ->
  TrapexitFlag = proplists:get_value(trap_exit,erlang:process_info(self())),
  process_flag(trap_exit, true), 
  Ret = gen_server:start_link(?MODULE, {ConnType, Host, Port, User, Pass}, []), 
  process_flag(trap_exit, TrapexitFlag), 
  Ret.

close_account(Account) ->
  gen_server:call(Account, close_account).

list(Account, RefName, Mailbox) ->
  gen_server:call(Account, {list, RefName, Mailbox}).

%% @link http://tools.ietf.org/html/rfc3501#section-6.3.10
%% The currently defined status data items that can be requested are:
%%   MESSAGES      --->  The number of messages in the mailbox.
%%   RECENT        --->  The number of messages with the \Recent flag set.
%%   UIDNEXT       --->  The next unique identifier value of the mailbox.
%%   UIDVALIDITY   --->  The unique identifier validity value of the mailbox.
%%   UNSEEN        --->  The number of messages which do not have the \Seen flag set.
status(Account, Mailbox, StatusDataItems) ->
  gen_server:call(Account, {status, Mailbox, StatusDataItems}).

select(Account, Mailbox) ->
  {ok, M} = gen_server:call(Account, {select, Mailbox}),
  {ok, M#mailbox{name = string:to_upper(Mailbox)}}.

examine(Account, Mailbox) ->
  gen_server:call(Account, {examine, Mailbox}).

%% "From: Fred Foobar <foobar@Blurdybloop.COM>\r\nSubject: afternoon meeting\r\n\r\nddd\r\n"
append(Account, Mailbox, Flags, Message) ->
  gen_server:call(Account, {append, Mailbox, Flags, Message}).

expunge(Account) ->
  gen_server:call(Account, expunge).

search(Account, SearchKeys) ->
  gen_server:call(Account, {search, SearchKeys}).

fetch(Account, SequenceSet, MsgDataItems) ->
  gen_server:call(Account, {fetch, SequenceSet, MsgDataItems}, infinity).

copy(Account, SequenceSet, Mailbox) ->
  gen_server:call(Account, {copy, SequenceSet, Mailbox}).

store(Account, SequenceSet, Flags, Action) ->
  gen_server:call(Account, {store, SequenceSet, Flags, Action}).

fsm_state(Account) ->
  gen_server:call(Account, fsm_state).
%%%-------------------
%%% Callback functions
%%%-------------------

init({ConnType, Host, Port, User, Pass}) ->
  try
    {ok, Conn} = case ConnType of
                   tcp -> imapc_fsm:connect(Host, Port);
                   ssl -> imapc_fsm:connect_ssl(Host, Port)
                 end,
    ok = imapc_fsm:login(Conn, User, Pass),
    {ok, Conn}
  catch
    error:{badmatch, {error, Reason}} -> {stop, Reason}
  end.

handle_call(close_account, _From, Conn) ->
  try
    ok = imapc_fsm:logout(Conn),
    ok = imapc_fsm:disconnect(Conn),
    {stop, normal, ok, Conn}
  catch
    error:{badmatch, {error, Reason}} -> {stop, Reason, {error, Reason}, Conn}
  end;
  
handle_call({list, RefName, Mailbox}, _From, Conn) ->
  {reply, imapc_fsm:list(Conn, RefName, Mailbox), Conn};
handle_call({status, Mailbox, StatusDataItems}, _From, Conn) ->
  {reply, imapc_fsm:status(Conn, Mailbox, StatusDataItems), Conn};
handle_call({select, Mailbox}, _From, Conn) ->
  {reply, imapc_fsm:select(Conn, Mailbox), Conn};
handle_call({examine, Mailbox}, _From, Conn) ->
  {reply, imapc_fsm:examine(Conn, Mailbox), Conn};
handle_call({append, Mailbox, Flags, Message}, _From, Conn) ->
  {reply, imapc_fsm:append(Conn, Mailbox, Flags, Message), Conn};
handle_call(expunge, _From, Conn) ->
  {reply, imapc_fsm:expunge(Conn), Conn};
handle_call({search, SearchKeys}, _From, Conn) ->
  {reply, imapc_fsm:search(Conn, SearchKeys), Conn};
handle_call({fetch, SequenceSet, MsgDataItems}, _From, Conn) ->
  {reply, imapc_fsm:fetch(Conn, SequenceSet, MsgDataItems), Conn};
handle_call({copy, SequenceSet, Mailbox}, _From, Conn) ->
  {reply, imapc_fsm:copy(Conn, SequenceSet, Mailbox), Conn};
handle_call({store, SequenceSet, Flags, Action}, _From, Conn) ->
  {reply, imapc_fsm:store(Conn, SequenceSet, Flags, Action), Conn};
handle_call(fsm_state, _From, Conn) ->
  {reply, imapc_fsm:fsm_state(Conn), Conn};
handle_call(_, _From, Conn) ->
  {reply, ignored, Conn}.



handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(Reason, _State) ->
  {error, Reason}.

%%%-----------
%%% tests
%%%-----------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.
