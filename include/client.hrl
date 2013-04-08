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




