-define(POP_PORT,110).
-define(POPS_PORT, 995).

-record(popc_fsm,{
                  socket = #socket{},
                  state = authorization,  % lower_case([AUTHORIZATION,TRANSACTION,UPDATE])
                  capability = [],
                  mailbox = [],
                  mail = [],
                  encrypt = plain,
                  peer = []
                 }).