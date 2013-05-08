%% Copyright (c) 2009-2010 Beijing RYTong Information Technologies, Ltd.
%% All rights reserved.
%%
%% No part of this source code may be copied, used, or modified
%% without the express written consent of RYTong.
-module(mail_client).
-behaviour(gen_server).

%%
%% Include files
%%

-include("client.hrl").
-include("imap.hrl").
-include("mimemail.hrl").

%%
%% Exported Functions
%%
%% Open and close POP3/IMAP4v1 retrieve session.
-export([open_retrieve_session/5, close_retrieve_session/1 ]).

%% SMTP ONLY
-export([open_send_session/5,
         close_send_session/1,
         send/8,
         send/7
        ]).

%% POP ONLY
-export([pop_capabilities/1, %% Recommended
         pop_list_size/1,
         pop_list/1,
         pop_retrieve/2,
         pop_retrieve/3,
         pop_list_top/1,
         pop_top/2,

         capabilities/1, %% Old pop3 API names
         list_size/1,
         list/1,
         retrieve/2,
         retrieve/3,
         list_top/1,
         top/2
    ]).

%% IMAP ONLY
-export([imap_list_mailbox/1, imap_list_mailbox/2,
         imap_select_mailbox/1, imap_select_mailbox/2, imap_select_mailbox/3,
         imap_save_draft/2]).
-export([imap_list_message/3,
         imap_retrieve_message/2, imap_retrieve_message/3,
         imap_seen_message/2,
         imap_trash_message/2,
         imap_move_message/3,
         imap_clear_mailbox/1
         ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {fsm, handler}).

%%
%% API Functions
%%


%% Retrieve APIs

%% Options = [imap | ssl | {timeout, integer()}]
open_retrieve_session(Host, Port, User, Passwd, Options) ->
    process_flag(trap_exit, true), 
    gen_server:start_link(?MODULE, {Host, Port, User, Passwd, Options}, []).

close_retrieve_session(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> gen_server:call(Pid, close);
            %popc:quit(Pid);
        _ -> ok
    end.


list_size(Pid) ->
    pop_list_size(Pid).
pop_list_size(Pid) ->
    gen_server:call(Pid, pop_list_size). 

list_top(Pid) ->
    pop_list_top(Pid).
pop_list_top(Pid) ->
    gen_server:call(Pid, pop_list_top). 

list(Pid) ->
    pop_list(Pid).
pop_list(Pid) ->
    gen_server:call(Pid, pop_list). 

retrieve(Pid, MessageID) ->
    pop_retrieve(Pid, MessageID).
retrieve(Pid, MessageID, Type) ->
    pop_retrieve(Pid, MessageID, Type).
pop_retrieve(Pid, MessageID) ->
    pop_retrieve(Pid, MessageID, plain).
pop_retrieve(Pid, MessageID, Type) ->
    gen_server:call(Pid, {pop_retrieve, MessageID, Type}). 

top(Pid, MessageID) ->
    pop_top(Pid, MessageID).
pop_top(Pid, MessageID) ->
    gen_server:call(Pid, {pop_top, MessageID}). 

%% Get pop3 server capabilities.
capabilities(Pid) ->
    pop_capabilities(Pid).
pop_capabilities(Pid) ->
    gen_server:call(Pid, pop_capabilities).


%% Send APIs
open_send_session(Server, Port, User, Passwd, Options) ->    
    {ok, Fsm} = smtpc:connect(Server, Port, Options),
    smtpc:ehlo(Fsm, "localhost"),
    ok = smtpc:auth(Fsm, User, Passwd),
    {ok, Fsm}.

close_send_session(Fsm) ->
    case erlang:is_process_alive(Fsm) of
        true ->
            smtpc:quit(Fsm),
            ok;
        _ ->
            ok
    end.

send(Fsm, From, To, Cc, Subject, Body, Attatchments) ->
    send(Fsm, From, To, Cc, [], Subject, Body, Attatchments).

send(Fsm, From, To, Cc, Bcc, Subject, Body, Attatchments) ->
    ?D(From),
    smtpc:mail(Fsm, From),
    [smtpc:rcpt(Fsm, Address)|| Address<-To],
    [smtpc:rcpt(Fsm, Address)|| Address<-Cc],
    [smtpc:rcpt(Fsm, Address)|| Address<-Bcc],
    Mail = send_util:encode_mail(From, To, Cc, Bcc, Subject, Body, Attatchments),
    ?D(Mail),
    smtpc:data(Fsm, binary_to_list(Mail)),
    ok.


%% List mailboxes with simple desc.
imap_list_mailbox(Pid) ->
    imap_list_mailbox(Pid, "\"\"").
imap_list_mailbox(Pid, RefName) when is_list(RefName)->
    gen_server:call(Pid, {imap_list_mailbox, RefName}). 

%% select mailbox and list messages.
imap_select_mailbox(Pid) ->
    imap_select_mailbox(Pid, "INBOX").
imap_select_mailbox(Pid, Mailbox) when is_list(Mailbox) -> 
    imap_select_mailbox(Pid, Mailbox, 5).
imap_select_mailbox(Pid, Mailbox, Num) when is_list(Mailbox), is_integer(Num), Num > 0 ->
    gen_server:call(Pid, {imap_select_mailbox, Mailbox, Num}). 

%% list messages with simple desc.
imap_list_message(Pid, FromSeq, ToSeq) when is_integer(FromSeq), is_integer(ToSeq), FromSeq =< ToSeq->
    gen_server:call(Pid, {imap_list_message, FromSeq, ToSeq}). 

%% retrieve message.
imap_retrieve_message(Pid, MsgSeq) when is_integer(MsgSeq)->
    imap_retrieve_message(Pid, MsgSeq, MsgSeq).
imap_retrieve_message(Pid, FromSeq, ToSeq) when is_integer(FromSeq), is_integer(ToSeq)->
    gen_server:call(Pid, {imap_retrieve_message, FromSeq, ToSeq}, infinity). 

%% RFC2822Msg SHOULD be in the format of an [RFC-2822] message.
imap_save_draft(Pid, RFC2822Msg) when is_list(RFC2822Msg) ->
    gen_server:call(Pid, {imap_save_draft, RFC2822Msg}). 

%% Set the \Seen flag.
imap_seen_message(Pid, MsgSeq) when is_integer(MsgSeq)->
    SeqSet = lists:concat([MsgSeq, ":", MsgSeq]),
    imap_seen_message(Pid, SeqSet);
imap_seen_message(Pid, SeqSet) when is_list(SeqSet) ->
    gen_server:call(Pid, {imap_seen_message, SeqSet}). 

%% Move Mails to Trash.
imap_trash_message(Pid, MsgSeq) when is_integer(MsgSeq)->
    SeqSet = lists:concat([MsgSeq, ":", MsgSeq]),
    imap_trash_message(Pid, SeqSet);
imap_trash_message(Pid, SeqSet) when is_list(SeqSet) ->
    gen_server:call(Pid, {imap_trash_message, SeqSet}). 

%% Move Mails to Other Mailbox.
imap_move_message(Pid, MsgSeq, Mailbox) when is_integer(MsgSeq), is_list(Mailbox)->
    SeqSet = lists:concat([MsgSeq, ":", MsgSeq]),
    imap_move_message(Pid, SeqSet, Mailbox);
imap_move_message(Pid, SeqSet, Mailbox) when is_list(SeqSet), is_list(Mailbox) ->
    gen_server:call(Pid, {imap_move_message, SeqSet, Mailbox}). 

%% Delete Mails that marked \Deleted.
imap_clear_mailbox(Pid) ->
    gen_server:call(Pid, imap_clear_mailbox). 

%%%-------------------
%%% Callback functions
%%%-------------------

init({Host, Port, User, Pass, Options}) ->
  try
    Handler = case lists:member(imap, Options) of
                true -> imapc;
                false -> popc
              end,
    {ok, Fsm} = Handler:connect(Host, Port, Options),
    ok = Handler:login(Fsm, User, Pass),
    {ok, #state{fsm=Fsm, handler=Handler}}
  catch
    error:{badmatch, {error, Reason}} -> {stop, Reason}
  end.

handle_call(close, _From, State = #state{fsm=Fsm, handler=Handler}) ->
  try
    ok = Handler:quit(Fsm),
    {stop, normal, ok, State}
  catch
    error:{badmatch, {error, Reason}} -> {stop, Reason, {error, Reason}, State}
  end;
handle_call(pop_list_size, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    Reply = 
        case Handler:list(Fsm) of
            {ok, RawList} ->
                {ok, get_total_number(RawList)};
            Err ->
                ?D(Err),
                Err
        end,
    {reply, Reply, State};
handle_call(pop_list_top, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    Reply = 
        case Handler:list(Fsm) of
            {ok, RawList} ->
                Num = get_total_number(RawList),
                ?D(Num),
                lists:map(fun(I) ->
                                  {ok, C} = popc:top(Fsm, I, 0),
                                  ?D({id, I}),
                                  {I, mimemail:decode_headers(C, <<"utf8">>)}
                          end, lists:seq(1, Num));
            Err ->
                ?D(Err),
                Err
        end,
    {reply, Reply, State};
handle_call(pop_list, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    Reply = 
        case Handler:list(Fsm) of
            {ok, RawList} ->
                Num = get_total_number(RawList),
                ?D(Num),
                lists:map(fun(I) ->
                                  {ok, C} = popc:retrieve(Fsm, I),
                                  ?D({id, I}),
                                  {I, retrieve_util:raw_message_to_mail(C)}
                          end, lists:seq(1, Num));
            Err ->
                ?D(Err),
                Err
        end,
    {reply, Reply, State};
handle_call({pop_retrieve, MessageID, Type}, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    Reply = 
        case Handler:retrieve(Fsm, MessageID) of
            {ok, RawMessage} ->
                retrieve_util:raw_message_to_mail(RawMessage, Type);
            Err ->
                ?D(Err),
                Err
        end,
    {reply, Reply, State};
handle_call({pop_top, MessageID}, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    Reply = 
        case Handler:top(Fsm, MessageID, 0) of
            {ok, RawMessage} ->
                mimemail:decode_headers(RawMessage, <<"utf8">>);
            Err ->
                ?D(Err),
                Err
        end,
    {reply, Reply, State};
handle_call(pop_capabilities, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    Reply = 
        case Handler:capa(Fsm) of
            {ok, RawList} ->
                {ok, parse_raw_list(RawList)};
            Err ->
                ?D(Err),
                Err
        end,
    {reply, Reply, State};
handle_call({imap_list_mailbox, RefName}, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    {ok, Mailboxes} = Handler:list(Fsm, RefName, "%"),
    Reply = {ok, lists:foldl(
        fun({Mailbox, Attrs}, Acc) ->
            {ok, [{Name, Value}]} = Handler:status(Fsm, Mailbox, "(unseen messages)"),
            [{imapc_util:mailbox_to_utf8(Mailbox), Name, [{attributes, Attrs}|Value]} | Acc]
        end, [], Mailboxes)}, 
    {reply, Reply, State};
handle_call({imap_select_mailbox, Mailbox, Num}, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    {ok, SelectedMailbox} = Handler:select(Fsm, Mailbox), 
    MsgSize = SelectedMailbox#mailbox.exists,
    FromSeq =
        if
            MsgSize =< Num -> 1;
            true -> (MsgSize - Num + 1)
        end,
    {ok, MessageList} = do_imap_list_message(Fsm, FromSeq, MsgSize),
    {reply, {ok, {SelectedMailbox, MessageList}}, State};
handle_call({imap_list_message, FromSeq, ToSeq}, _From, State = #state{fsm=Fsm}) ->
    {reply, do_imap_list_message(Fsm, FromSeq, ToSeq), State};
handle_call({imap_retrieve_message, FromSeq, ToSeq}, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    SeqSet = lists:concat([FromSeq, ":", ToSeq]),
    {ok, RawMessageList} = Handler:fetch(Fsm, SeqSet, "(rfc822)"),
    ParsedMessageList = lists:map(
        fun({Seq, Content}) ->
            {ok, Raw} = imapc_util:parse_fetch_result("RFC822", Content),
            {Seq, retrieve_util:raw_message_to_mail(Raw)}
        end, RawMessageList), 
    {reply, {ok, ParsedMessageList}, State};
handle_call({imap_save_draft, MailText}, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    Reply = Handler:append(Fsm, "\\Drafts", "()", MailText),
    {reply, Reply, State};
handle_call({imap_seen_message, SeqSet}, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    Reply = Handler:store(Fsm, SeqSet, "+FLAGS", "(\\Seen)"),
    {reply, Reply, State};
handle_call({imap_trash_message, SeqSet}, _From, State = #state{fsm=Fsm}) ->
    Reply = do_imap_trash_message(Fsm, SeqSet),
    {reply, Reply, State};
handle_call({imap_move_message, SeqSet, Mailbox}, _From, State = #state{fsm=Fsm, handler=Handler}) ->
    Handler:copy(Fsm, SeqSet, Mailbox),
    do_imap_trash_message(Fsm, SeqSet),
    Reply = do_imap_clear_mailbox(Fsm),
    {reply, Reply, State};
handle_call(imap_clear_mailbox, _From, State = #state{fsm=Fsm}) ->
    Reply = do_imap_clear_mailbox(Fsm),
    {reply, Reply, State};

handle_call(_, _From, Fsm) ->
  {reply, ignored, Fsm}.

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

%%
%% Local Functions
%%

get_total_number(Raw) ->
    Index = string:str(Raw, ?CRLF),
    [Num|_] = string:tokens(string:substr(Raw, 1, Index -1), " "),
    list_to_integer(Num).

parse_raw_list(Raw) ->
    string:tokens(Raw, "\r\n").

do_imap_list_message(Fsm, FromSeq, ToSeq) ->
    SeqSet = lists:concat([FromSeq, ":", ToSeq]),
    DataItems = "(flags envelope bodystructure rfc822.size)",
    {ok, MessageList} = imapc:fetch(Fsm, SeqSet, DataItems),
    MessageList2 = lists:map(
        fun({Seq, Content}) ->
            {ok, Envelope} = imapc_util:parse_fetch_result("ENVELOPE", Content),
            HasAttachment = imapc_util:parse_fetch_result("HAS_ATTACHEMENT", Content),
            {ok, Size} = imapc_util:parse_fetch_result("RFC822.SIZE", Content),
            {ok, Flags} = imapc_util:parse_fetch_result("FLAGS", Content),
            {Seq, [{"HAS_ATTACHEMENT", HasAttachment}, {"SIZE", Size}, {"FLAGS", Flags} | Envelope]}
        end, MessageList), 
    {ok, MessageList2}.

do_imap_trash_message(Fsm, SeqSet) ->
    imapc:store(Fsm, SeqSet, "+FLAGS", "(\\Deleted)").

do_imap_clear_mailbox(Fsm) ->
    imapc:expunge(Fsm).

