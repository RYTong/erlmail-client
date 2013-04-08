-define(SMTP_DATA_END, [13,10,46,13,10]). % End of data command "\r\n.\r\n"

-record(smtpc,{
               socket = [],
               auth = [],
               features = [],
               type = smtp, % smtp server type: [smtp:esmtp]
               state = helo % State of command, [helo,mail,rcpt,data]
              }).
