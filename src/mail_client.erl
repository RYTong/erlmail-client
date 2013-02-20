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
-include("mimemail.hrl").

%%
%% Exported Functions
%%
-export([open_retrieve_session/5,
         close_retrieve_session/1,
         list/1,
         retrieve/2,
         retrieve/3,
         open_send_session/5,
         close_send_session/1,
         send/7
        ]).


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
    ?D(From),
    smtpc:mail(Fsm, From),
    [smtpc:rcpt(Fsm, Address)|| Address<-To],
    [smtpc:rcpt(Fsm, Address)|| Address<-Cc],
    Mail = encode_mail(From, To, Cc, Subject, Body, Attatchments),
    ?D(Mail),
    smtpc:data(Fsm, binary_to_list(Mail)),
    ok.


%%
%% API Functions
%%





%%
%% Local Functions
%%

get_total_number(Raw) ->
    Index = string:str(Raw, ?CRLF),
    [Num|_] = string:tokens(string:substr(Raw, 1, Index -1), " "),
    list_to_integer(Num).

encode_mail(From, To, Cc, Subject, Body, []) ->
    ToList = [{<<"To">>, list_to_binary(Address)}|| Address <-To],
    CcList = [{<<"Cc">>, list_to_binary(Address)}|| Address <-Cc],
    Headers = [{<<"From">>, list_to_binary(From)},
               {<<"Subject">>, list_to_binary(Subject)},
               {<<"MIME-Version">>, <<"1.0">>}] ++ ToList ++ CcList,
    Email = {<<"text">>, <<"plain">>, Headers,
             [{<<"content-type-params">>,
               [{<<"charset">>,<<"UTF-8">>}],
               {<<"disposition">>,<<"inline">>}}],
             list_to_binary(Body)},
    mimemail:encode(Email);

encode_mail(From, To, Cc, Subject, Body, Attatchments) ->
    ToList = [{<<"To">>, list_to_binary(Address)}|| Address <-To],
    CcList = [{<<"Cc">>, list_to_binary(Address)}|| Address <-Cc],
    Headers = [{<<"From">>, list_to_binary(From)},
               {<<"Subject">>, list_to_binary(Subject)},
               {<<"MIME-Version">>, <<"1.0">>}] ++ ToList ++ CcList,
    BodyPart = {<<"text">>, <<"plain">>, Headers,
                [{<<"content-type-params">>,
                  [{<<"charset">>,<<"UTF-8">>}],
                  {<<"disposition">>,<<"inline">>}}],
                list_to_binary(Body)},
    AttachPart = attach_to_mime(Attatchments, []),
    MimeMail = {<<"multipart">>, <<"mixed">>, Headers,
                [{<<"content-type-params">>,
                  [{<<"charset">>,<<"UTF-8">>}],
                  {<<"disposition">>,<<"inline">>}}],
                [BodyPart|AttachPart]},
    mimemail:encode(MimeMail).






%% attachment
attach_to_mime([], Res) ->
    lists:reverse(Res);
attach_to_mime([{Name, Content}|T], Res) ->
    attach_to_mime(T, [attachment(Name, Content)|Res]).

attachment(FileName, Content) when is_list(FileName) ->
    attachment(list_to_binary(FileName), Content);
attachment(FileName, Content) when is_list(Content) ->
    attachment(FileName, list_to_binary(Content));
attachment(FileName, Content) when is_binary(FileName), is_binary(Content) ->
    {Type, Subtype, Render} = from_ext(FileName),
    Headers = [{<<"Content-Disposition">>,
                <<Render/binary, ";filename=", FileName/binary>>},
               {<<"Content-Type">>,
                <<"text/plain">>}],
    Properties = [{<<"content-type-params">>,
                   [{<<"name">>,FileName}]},
                  {<<"disposition">>,Render},
                  {<<"disposition-params">>,
                   [{<<"filename">>,FileName}]}],
    {Type, Subtype, Headers, Properties, Content}.

from_ext(FileName) ->
    ?D(FileName),
    to_do,
    {<<"text">>, <<"plain">>, <<"attachment">>}.





    
           


