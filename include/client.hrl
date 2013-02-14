

-ifdef(debug).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-endif.

-define(POP_PORT,110).
-define(POPS_PORT, 995).

-ifndef(CRLF).
-define(CRLF,[13,10]).
-define(CR, 13).
-define(LF, 10).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.

-define(SMTP_DATA_END, [13,10,46,13,10]). % End of data command "\r\n.\r\n"
-define(TIMEOUT,   300000).

-record(socket, {fd,
                 type,
                 timeout = ?TIMEOUT}).

-record(popc_fsm,{
                  socket = #socket{},
                  state = authorization,  % lower_case([AUTHORIZATION,TRANSACTION,UPDATE])
                  capability = [],
                  mailbox = [],
                  mail = [],
                  encrypt = plain,
                  peer = []
                 }).

-record(smtpc,{
               socket = [],
               auth = [],
               features = [],
               type = smtp, % smtp server type: [smtp:esmtp]
               state = helo % State of command, [helo,mail,rcpt,data]
              }).

-record(attachment, {name,
                     type,
                     subtype,
                     content,
                     render  %% <<"inline">>|<<"attachment">>
                    }).

-record(mail, {from,
               to,
               date,
               id,
               subject,
               content,
               attachements %% [attachment()]
  }).




