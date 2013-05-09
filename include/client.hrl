-ifdef(debug).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-else.
-define(D(X), undefined).
-endif.

-ifndef(CRLF).
-define(CRLF,[13,10]).
-define(CR, 13).
-define(LF, 10).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.

-define(TIMEOUT,   300000).


-record(socket, {fd,
                 type,
                 timeout = ?TIMEOUT}).

-record(attachment, {name,
                     type,
                     subtype,
                     content,
                     render  %% <<"inline">>|<<"attachment">>
                    }).

-record(mail, {from,       %% binary()
               to,         %% [binary()]
               cc,         %% [binary()]
               bcc,
               date,       %% binary()
               id,         %% binary()
               subject,    %% binary()
               content,    %% binary() | [binary()] |{html, binary()} |[binary()|{html, binary()}]
               attachments  = []%% [attachment()]
              }).




% envelope        = "(" env-date SP env-subject SP env-from SP
%                   env-sender SP env-reply-to SP env-to SP env-cc SP
%                   env-bcc SP env-in-reply-to SP env-message-id ")"
% env-bcc         = "(" 1*address ")" / nil
% env-cc          = "(" 1*address ")" / nil
% env-date        = nstring
% env-from        = "(" 1*address ")" / nil
% env-in-reply-to = nstring
% env-message-id  = nstring
% env-reply-to    = "(" 1*address ")" / nil
% env-sender      = "(" 1*address ")" / nil
% env-subject     = nstring
% env-to          = "(" 1*address ")" / nil
% nil             = "NIL"
% nstring         = string / nil
-record(envelope, {
    env_date,
    env_subject,
    env_from,
    env_sender,
    env_reply_to,
    env_to,
    env_cc,
    env_bcc,
    env_in_reply_to,
    env_message_id
    }).


% address         = "(" addr-name SP addr-adl SP addr-mailbox SP
%                   addr-host ")"
% addr-adl        = nstring
%                     ; Holds route from [RFC-2822] route-addr if
%                     ; non-NIL
% addr-host       = nstring
%                     ; NIL indicates [RFC-2822] group syntax.
%                     ; Otherwise, holds [RFC-2822] domain name
% addr-mailbox    = nstring
%                     ; NIL indicates end of [RFC-2822] group; if
%                     ; non-NIL and addr-host is NIL, holds
%                     ; [RFC-2822] group name.
%                     ; Otherwise, holds [RFC-2822] local-part
%                     ; after removing [RFC-2822] quoting
% addr-name       = nstring
%                     ; If non-NIL, holds phrase from [RFC-2822]
%                     ; mailbox after removing [RFC-2822] quoting
-record(address, {
    addr_name,
    addr_adl,
    addr_mailbox,
    addr_host
  }).