%% Copyright (c) 2009-2010 Beijing RYTong Information Technologies, Ltd.
%% All rights reserved.
%%
%% No part of this source code may be copied, used, or modified
%% without the express written consent of RYTong.
-module(retrieve_util).

%%
%% Include files
%%


-include("client.hrl").
-include("mimemail.hrl").

%%
%% Exported Functions
%%
-export([raw_message_to_mail/1,
         raw_message_to_mail/2]).

%%
%% API Functions
%%

raw_message_to_mail(RawMessage) ->
    raw_message_to_mail(RawMessage, plain).

raw_message_to_mail(RawMessage, SubTypeWanted) when is_list(RawMessage) ->
    raw_message_to_mail(list_to_binary(RawMessage), SubTypeWanted);
raw_message_to_mail(RawMessage, SubTypeWanted) when is_binary(RawMessage) ->
    {Type, SubType, Headers, Properties, Body} = try
                                                     mimemail:decode((RawMessage), [{encoding, <<"utf8">>}])
                                                 catch ErrType:Err ->
                                                           ?D({ErrType, Err}),
                                                           mimemail:decode(RawMessage, [{encoding, none}])
                                                 end,
                                                           
    TypeWanted = case SubTypeWanted of
                     html -> <<"html">>;
                     _ -> <<"plain">>
                 end,
    mime_to_mail(#mimemail{type = Type, 
                           subtype = SubType,
                           headers = Headers,
                           properties = Properties, 
                           body = decode_body(Body)}, TypeWanted).


%%
%% Local Functions
%%

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

%% Parse MIME mail into a mail record,
%% filter out useless info.


%% Case of text/plain
mime_to_mail(#mimemail{type = <<"text">>,
                       subtype = <<"plain">>,
                       headers = Headers,
                       body = Body} = Mime, _TypeWanted) when is_binary(Body) ->
    ?D(Mime),
    Mail = get_headers(Headers),
    Mail#mail{content = Body};

%% If subtype is alternative, we fetch "text/plain" as our mail content
%% we may extend "text/html" later to get better user experience.
mime_to_mail(#mimemail{type = <<"multipart">>,
                       subtype = <<"alternative">>,
                       headers = Headers,
                       body = Body} = Mime, TypeWanted) when is_list(Body) ->
    ?D(Mime),
    Mail = get_headers(Headers),
    Mail#mail{content = get_text(Body, TypeWanted)};

mime_to_mail(#mimemail{type = <<"multipart">>,
                       %% subtype = <<"mixed">>, %% accroding to rfc2046, default subtype is mixed
                       headers = Headers,
                       body = Body} = Mime, TypeWanted) when is_list(Body) ->
    ?D(Mime),
    Mail = get_headers(Headers),                
    {Content, Attatchments} =  parse_body(Body, {[], []}, TypeWanted),
    Mail#mail{content = Content,
              attachments = Attatchments};

%% Only an attachment
mime_to_mail(#mimemail{headers = Headers} = Mime, TypeWanted) ->
    ?D(Mime),
    Mail = get_headers(Headers),
    {Content, Attatchments} = parse_body([Mime], {[], []}, TypeWanted),
    Mail#mail{content = Content,
              attachments = Attatchments}.

get_headers(Headers) ->
    #mail{from = proplists:get_value(<<"From">>, Headers),
          to = proplists:get_all_values(<<"To">>, Headers),
          cc = proplists:get_all_values(<<"Cc">>, Headers) ++ 
                   proplists:get_all_values(<<"CC">>, Headers),
          bcc = proplists:get_all_values(<<"Bcc">>, Headers),
          date = proplists:get_value(<<"Date">>, Headers),
          id = proplists:get_value(<<"Message-ID">>, Headers),
          subject = proplists:get_value(<<"Subject">>, Headers)
         }.


get_text(Mimemails, Subtype) ->
    get_text(Mimemails, <<>>, Subtype).



get_text([], R, _) ->
    R;
get_text([#mimemail{type = <<"text">>,
                    subtype = <<"plain">>,
                    body = Body}|_T], _R, <<"plain">>) ->
    Body;
get_text([#mimemail{type = <<"text">>,
                    subtype = <<"html">>,
                    body = Body}|_T], _R, <<"html">>) ->
    {html, Body};
get_text([_H|T], R, Subtype) ->
    get_text(T, R, Subtype).

%% Parse the nested body

parse_body([], {Content, AttachList}, <<"html">>) ->
    {lists:reverse(Content), lists:reverse(AttachList)};  
%% plain text is the wanted type
parse_body([], {Content, AttachList}, <<"plain">>) ->
    {append(Content,<<>>), lists:reverse(AttachList)}; 
parse_body([#mimemail{type = <<"text">>,
                      subtype = <<"plain">>,
                      headers = Headers,
                      properties = Properties,
                      body = Body}|T], {Content, AttachList}, Subtype) when is_binary(Body) ->
    case get_filename(Headers, Properties) of 
        {_, undefined} -> parse_body(T, {[Body|Content], AttachList}, Subtype);
        {Inline, Filename} ->
            Attach = #attachment{type = <<"text">>,
                                 subtype = <<"plain">>,
                                 name = Filename,
                                 content = Body,
                                 render = Inline},
            parse_body(T, {Content, [Attach|AttachList]}, Subtype)
    end;
parse_body([#mimemail{type = <<"multipart">>,
                      subtype = <<"alternative">>,
                      body = Body}|T], {Content, AttachList}, Subtype) ->
    Text = get_text(Body, Subtype),
    %%     ?D({text, <<Content/binary, Text/binary>>, T}),
    parse_body(T, {[Text|Content], AttachList}, Subtype);

%% 
parse_body([#mimemail{type = <<"multipart">>,
                      body = Body}|T], {Content, AttachList}, Subtype) ->
    R = parse_body(Body, {Content, AttachList}, Subtype),
    parse_body(T, R, Subtype);

parse_body([#mimemail{type = <<"image">>,
                      subtype = ImageType,
                      headers = Headers,
                      properties = Properties,
                      body = Body}|T], {Content, AttachList}, Subtype) ->
    {Inline, Filename} = get_filename(Headers, Properties),
    ImageAttachment = #attachment{type = <<"image">>,
                                  subtype = ImageType,
                                  name = Filename,
                                  content = Body,
                                  render = Inline},
    parse_body(T, {Content, [ImageAttachment|AttachList]}, Subtype);
parse_body([#mimemail{type = Type,
                      subtype = SubType,
                      headers = Headers,
                      properties = Properties,
                      body = Body} = Mime|T], {Content, AttachList}, Subtype) ->
    Attachment = case get_filename(Headers, Properties) of
                     {_, undefined} ->
                         ?D({skip_mimemail, Mime}),
                         [];
                     {Inline, Filename}  ->
                         #attachment{type = Type,
                                     subtype = SubType,
                                     name = Filename,
                                     content = Body,
                                     render = Inline}
                 end,
    parse_body(T, {Content, [Attachment|AttachList]}, Subtype);
parse_body([Mime|T], R, Subtype) ->
    ?D({wrong_mimemail, Mime}),
    parse_body(T, R, Subtype).

append([], R) ->
    R;
append([H|T], R) ->
    append(T, <<R/binary, H/binary>>).

%% Get filename from Properties or Headers

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
            undefined;
        B -> 
            ?D(B),
            case string:tokens(binary_to_list(B), ";") of
                ["attachment"|Params] ->
                    get_filename(Params);
                _ ->
                    undefined 
            end
    end.

get_filename(["filename=" ++ Name|_T]) ->
    list_to_binary(Name);
get_filename([]) ->
    <<"attachment">>;
get_filename([_H|T]) ->
    get_filename(T).


