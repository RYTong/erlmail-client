%% Copyright (c) 2009-2010 Beijing RYTong Information Technologies, Ltd.
%% All rights reserved.
%%
%% No part of this source code may be copied, used, or modified
%% without the express written consent of RYTong.
-module(mail_client).

%%
%% Include files
%%

-include("client.hrl").
-include("imap.hrl").
-include("mimemail.hrl").

%%
%% Exported Functions
%%
-export([open_retrieve_session/5,
         close_retrieve_session/1,
         pop_capabilities/1,
         list_size/1,
         list/1,
         retrieve/2,
         retrieve/3,
         list_top/1,
         top/2,
         open_send_session/5,
         close_send_session/1,
         send/8,
         send/7
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


%%
%% API Functions
%%


%% Retrieve APIs

%% Options = [ssl | {timeout, integer()}]

open_retrieve_session(Server, Port, User, Passwd, Options) ->
    {ok, Fsm} = popc:connect(Server, Port, Options),
    ok = popc:login(Fsm, User, Passwd),
    {ok, Fsm}.

close_retrieve_session(Fsm) ->
    case erlang:is_process_alive(Fsm) of
        true ->
            popc:quit(Fsm);
        _ ->
            ok
    end.


list_size(Fsm) ->
    case popc:list(Fsm) of
        {ok, RawList} ->
            {ok, get_total_number(RawList)};
        Err ->
            ?D(Err),
            Err
    end.

list_top(Fsm) ->
    case popc:list(Fsm) of
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
    end.

list(Fsm) ->
    case popc:list(Fsm) of
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
    end.

retrieve(Fsm, MessageId) ->
    retrieve(Fsm, MessageId, plain).

retrieve(Fsm, MessageId, Type) ->
    case popc:retrieve(Fsm, MessageId) of
        {ok, RawMessage} ->
            retrieve_util:raw_message_to_mail(RawMessage, Type);
        Err ->
            ?D(Err),
            Err
    end.

top(Fsm, MessageId) ->
    case popc:top(Fsm, MessageId, 0) of
        {ok, RawMessage} ->
            mimemail:decode_headers(RawMessage, <<"utf8">>);
        Err ->
            ?D(Err),
            Err
    end.

%% Get pop3 server capabilities.
pop_capabilities(Fsm) ->
    case popc:capa(Fsm) of
        {ok, RawList} ->
            {ok, parse_raw_list(RawList)};
        Err ->
            ?D(Err),
            Err
    end.

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
    {ok, Mailboxes} = imapc:list(Pid, RefName, "%"),
    lists:foldl(
        fun({Mailbox, Attrs}, Acc) ->
            {ok, Value} = imapc:status(Pid, Mailbox, "(unseen messages)"),
            [{Mailbox, [{attributes, Attrs}|Value]} | Acc]
        end, [], Mailboxes). 

%% select mailbox and list messages.
imap_select_mailbox(Pid) ->
    imap_select_mailbox(Pid, "INBOX").
imap_select_mailbox(Pid, Mailbox) when is_list(Mailbox) -> 
    imap_select_mailbox(Pid, Mailbox, 5).
imap_select_mailbox(Pid, Mailbox, Num) when is_list(Mailbox), is_integer(Num), Num > 0 ->
    {ok, SelectedMailbox} = imapc:select(Pid, Mailbox), 
    MsgSize = SelectedMailbox#mailbox.exists,
    FromSeq =
        if
            MsgSize =< Num -> 1;
            true -> (MsgSize - Num -1) 
        end,
    {ok, MessageList} = imap_list_message(Pid, FromSeq, MsgSize),
    {ok, {SelectedMailbox, MessageList}}.

%% list messages with simple desc.
imap_list_message(Pid, FromSeq, ToSeq) when is_integer(FromSeq), is_integer(ToSeq), FromSeq =< ToSeq->
    SeqSet = lists:concat([FromSeq, ":", ToSeq]),
    DataItems = "(flags envelope bodystructure rfc822.size)",
    {ok, MessageList} = imapc:fetch(Pid, SeqSet, DataItems),
    MessageList2 = lists:map(
        fun({Seq, Content}) ->
            {ok, Envelope} = imapc_util:parse_fetch_result("ENVELOPE", Content),
            HasAttachment = imapc_util:parse_fetch_result("HAS_ATTACHEMENT", Content),
            {ok, Size} = imapc_util:parse_fetch_result("RFC822.SIZE", Content),
            {ok, Flags} = imapc_util:parse_fetch_result("FLAGS", Content),
            {Seq, [{"HAS_ATTACHEMENT", HasAttachment}, {"SIZE", Size}, {"FLAGS", Flags} | Envelope]}
        end, MessageList), 
    {ok, MessageList2}.

%% retrieve message.
imap_retrieve_message(Pid, MsgSeq) when is_integer(MsgSeq)->
    imap_retrieve_message(Pid, MsgSeq, MsgSeq).
imap_retrieve_message(Pid, FromSeq, ToSeq) when is_integer(FromSeq), is_integer(ToSeq)->
    SeqSet = lists:concat([FromSeq, ":", ToSeq]),
    {ok, RawMessageList} = imapc:fetch(Pid, SeqSet, "(rfc822)"),
    ParsedMessageList = lists:map(
        fun({Seq, Content}) ->
            {ok, Raw} = imapc_util:parse_fetch_result("RFC822", Content),
            {Seq, retrieve_util:raw_message_to_mail(Raw)}
        end, RawMessageList), 
    {ok, ParsedMessageList}.

%% RFC2822Msg SHOULD be in the format of an [RFC-2822] message.
imap_save_draft(Pid, RFC2822Msg) when is_list(RFC2822Msg) ->
    imapc:append(Pid, "\\Drafts", "()", RFC2822Msg).

%% Set the \Seen flag.
imap_seen_message(Pid, MsgSeq) when is_integer(MsgSeq)->
    SeqSet = lists:concat([MsgSeq, ":", MsgSeq]),
    imap_seen_message(Pid, SeqSet);
imap_seen_message(Pid, SeqSet) when is_list(SeqSet) ->
    imapc:store(Pid, SeqSet, "+FLAGS", "(\\Seen)").

%% Move Mails to Trash.
imap_trash_message(Pid, MsgSeq) when is_integer(MsgSeq)->
    SeqSet = lists:concat([MsgSeq, ":", MsgSeq]),
    imap_trash_message(Pid, SeqSet);
imap_trash_message(Pid, SeqSet) when is_list(SeqSet) ->
    imapc:store(Pid, SeqSet, "+FLAGS", "(\\Deleted)").

%% Move Mails to Other Mailbox.
imap_move_message(Pid, MsgSeq, Mailbox) when is_integer(MsgSeq), is_list(Mailbox)->
    SeqSet = lists:concat([MsgSeq, ":", MsgSeq]),
    imap_move_message(Pid, SeqSet, Mailbox);
imap_move_message(Pid, SeqSet, Mailbox) when is_list(SeqSet), is_list(Mailbox) ->
    imapc:copy(Pid, SeqSet, Mailbox),
    imap_trash_message(Pid, SeqSet),
    imap_clear_mailbox(Pid).

%% Delete Mails that marked \Deleted.
imap_clear_mailbox(Pid) ->
    imapc:expunge(Pid).

%%
%% Local Functions
%%

get_total_number(Raw) ->
    Index = string:str(Raw, ?CRLF),
    [Num|_] = string:tokens(string:substr(Raw, 1, Index -1), " "),
    list_to_integer(Num).

parse_raw_list(Raw) ->
    string:tokens(Raw, "\r\n").





    
           


