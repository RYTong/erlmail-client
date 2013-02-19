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
         open_send_session/5,
         close_send_session/1,
         send/7
        ]).

-export([raw_message_to_mail/1,
         mime_to_mail/1]).

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
                              {I, raw_message_to_mail(C)}
                      end, lists:seq(1, Num));
        Err ->
            ?D(Err),
            Err
    end.

retrieve(Fsm, MessageId) ->
    case popc:retrieve(Fsm, MessageId) of
        {ok, RawMessage} ->
            raw_message_to_mail(RawMessage);
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



raw_message_to_mail(RawMessage) when is_list(RawMessage) ->
    {Type, SubType, Headers, Properties, Body} = 
        mimemail:decode(list_to_binary(RawMessage), [{encoding, <<"utf8">>}]),
    mime_to_mail(#mimemail{type = Type, 
                           subtype = SubType,
                           headers = Headers,
                           properties = Properties, 
                           body = decode_body(Body)}).

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

decode_body({Type, SubType, Headers, Properties, Body}) ->
    #mimemail{type = Type, 
              subtype = SubType,
              headers = Headers,
              properties = Properties, 
              body = decode_body(Body)};
decode_body(Body) when is_binary(Body) ->
    Body;
decode_body(Body) when is_list(Body) ->
    [decode_body(X)|| X <-Body].


mime_to_mail(#mimemail{headers = Headers,
                       body = Body} = Mime) when is_binary(Body) ->
    ?D(Mime),
    Mail = get_headers(Headers),
    Mail#mail{content = Body};

%% If subtype is alternative, we fetch "text/plain" as our mail content
%% we may extend "text/html" later to get better user experience.
mime_to_mail(#mimemail{type = <<"multipart">>,
                       subtype = <<"alternative">>,
                       headers = Headers,
                       body = Body} = Mime) when is_list(Body) ->
    ?D(Mime),
    Mail = get_headers(Headers),
    Mail#mail{content = get_plain_text(Body, <<"">>)};

mime_to_mail(#mimemail{type = <<"multipart">>,
                       %% subtype = <<"mixed">>, %% accroding to rfc2046, default subtype is mixed
                       headers = Headers,
                       body = Body} = Mime) when is_list(Body) ->
    ?D(Mime),
    Mail = get_headers(Headers),
    {Content, Attatchments} = 
        parse_body(Body, {<<"">>, []}),
    Mail#mail{content = Content,
              attachments = Attatchments};
mime_to_mail(#mimemail{headers = Headers,
                       body = Body} = Mime) ->
    ?D(Mime),
    Mail = get_headers(Headers),
    {Content, Attatchments} = 
        parse_body([Body], {<<"">>, []}),
    Mail#mail{content = Content,
              attachments = Attatchments}.

get_headers(Headers) ->
    #mail{from = proplists:get_value(<<"From">>, Headers),
          to = proplists:get_all_values(<<"To">>, Headers),
          cc = proplists:get_all_values(<<"Cc">>, Headers),
          date = proplists:get_value(<<"Date">>, Headers),
          id = proplists:get_value(<<"Message-ID">>, Headers),
          subject = proplists:get_value(<<"Subject">>, Headers)
         }.

get_plain_text([], R) ->
    R;
get_plain_text([#mimemail{type = <<"text">>,
                          subtype = <<"plain">>,
                          body = Body}|_T], _) ->
    Body;
get_plain_text([#mimemail{body = Body}|T], _) when is_binary(Body) ->
    get_plain_text(T, Body);
get_plain_text([_H|T], R) ->
    get_plain_text(T, R).

parse_body([], R) ->
    R;   
parse_body([#mimemail{type = <<"text">>,
                      subtype = <<"plain">>,
                      body = Body}|T], {Content, AttachList}) when is_binary(Body) ->
    parse_body(T, {<<Content/binary, Body/binary>>, AttachList});
parse_body([#mimemail{type = <<"multipart">>,
                      subtype = <<"alternative">>,
                      body = Body}|T], {Content, AttachList}) ->
    Text = get_plain_text(Body, <<>>),
%%     ?D({text, <<Content/binary, Text/binary>>, T}),
    parse_body(T, {<<Content/binary, Text/binary>>, AttachList});

%% 
parse_body([#mimemail{type = <<"multipart">>,
                      body = Body}|T], {Content, AttachList}) ->
    R = parse_body(Body, {Content, AttachList}),
    parse_body(T, R);

parse_body([#mimemail{type = <<"image">>,
                      subtype = ImageType,
                      headers = Headers,
                      properties = Properties,
                      body = Body}|T], {Content, AttachList}) ->
    {Inline, Filename} = get_filename(Headers, Properties),
    ImageAttachment = #attachment{type = <<"image">>,
                                  subtype = ImageType,
                                  name = Filename,
                                  content = Body,
                                  render = Inline},
    parse_body(T, {Content, [ImageAttachment|AttachList]});
parse_body([#mimemail{type = Type,
                      subtype = SubType,
                      headers = Headers,
                      properties = Properties,
                      body = Body}|T], {Content, AttachList}) ->
    {Inline, Filename} = get_filename(Headers, Properties),
    Attachment = #attachment{type = Type,
                             subtype = SubType,
                             name = Filename,
                             content = Body,
                             render = Inline},
    parse_body(T, {Content, [Attachment|AttachList]});
parse_body([Mime|T], R) ->
    ?D({skip_mimemail, Mime}),
    parse_body(T, R).

get_filename(Headers, Properties) ->
    Inline = proplists:get_value(<<"disposition">>, Properties, <<"attachment">>),
    Filename = case proplists:get_value(<<"disposition-params">>, Properties) of 
                   undefined -> parse_header(Headers);
                   Params ->
                       ?D(Params),
                       case proplists:get_value(<<"filename">>, Params) of
                           undefined -> parse_header(Headers);
                           Name -> Name 
                       end
               end,
    {Inline, Filename}.

parse_header(Headers) ->
    case  proplists:get_value(<<"Content-Disposition">>, Headers) of
        undefined ->
            <<"anonymous">>;
        B -> 
            ?D(B),
            [_Inline, File|_] = string:tokens(binary_to_list(B), ";"),
            "filename=" ++ Name = File,
            list_to_binary(Name)
    end.

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





    
           


