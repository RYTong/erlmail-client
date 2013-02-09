

-ifdef(debug).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-endif.

-define(POP_PORT,110).

-ifndef(CRLF).
-define(CRLF,[13,10]).
-define(CR, 13).
-define(LF, 10).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.



-record(popc_fsm,{
    socket = [],
    socket_type,
    state = authorization,  % [AUTHORIZATION,TRANSACTION,UPDATE]
    capability = [],
    mailbox = [],
    mail = [],
    encrypt = plain,
    peer = []
    }).

-define(SMTP_DATA_END, [13,10,46,13,10]). % End of data command "\r\n.\r\n"
-define(TIMEOUT,   300000).

-record(smtpc,{
               socket = [],
               auth = [],
               features = [],
               type = smtp, % smtp server type: [smtp:esmtp]
               state = helo % State of command, [helo,mail,rcpt,data]
              }).


