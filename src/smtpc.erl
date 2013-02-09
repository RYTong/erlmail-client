%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       SMTP Client API - Use these funcations instead of the FSM
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.1
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stuart Jackson, Simple Enigma, Inc. All Righs Reserved
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(smtpc).
-author('sjackson@simpleenigma.com').
-include("client.hrl").

-export([connect/1,help/1,noop/1,quit/1,rcpt/2,rset/1,vrfy/2]).
-export([connect/3,data/2,ehlo/2, auth/3, etrn/2,expn/2,helo/2,mail/2]).

-export([sendmail/10]).
-export([sendmail_test/6]).

%%-------------------------------------------------------------------------
%% @spec (IpAddress::term()) -> {ok,Pid::pid()} | {error,Reason::atom()}
%% @doc  Connects to an SMTP server and starts a FSM
%% @end
%%-------------------------------------------------------------------------
connect(IPAddress) -> connect(IPAddress,25, []).
%%-------------------------------------------------------------------------
%% @spec (IpAddress::term(),Port::integer()) -> {ok,Pid::pid()} | {error,Reason::atom()}
%% @doc  Connects to an SMTP server and starts a FSM
%% @end
%%-------------------------------------------------------------------------
connect(IPAddress,Port, Options) -> smtpc_fsm:start(IPAddress,Port, Options).

%%--------------------------------------------------------------------
%% Function: data(Pid,Message)
%%           Pid     = pid()
%%           Message = String() - Full Email Mesage to send
%% Descrip.: Sends data of email message
%% Returns : {Code,Response} - {250,String}
%%--------------------------------------------------------------------
data(Pid,Message) ->  gen_fsm:sync_send_event(Pid, {data,Message}, infinity).

%%--------------------------------------------------------------------
%% Function: ehlo(Pid,HostName)
%%           Pid      = pid()
%%           HostName = String() - Your Server Name
%% Descrip.: EHLO handshake
%% Returns : {Code,Response} - {250,String}
%%--------------------------------------------------------------------
ehlo(Pid,HostName) -> gen_fsm:sync_send_event(Pid, {ehlo,HostName}, infinity).

%%--------------------------------------------------------------------
%% Function: etrn(Pid,Message)
%%           Pid   = pid()
%%           Queue = String()
%% Descrip.: Asks server to process message in Queue
%% Returns : {Code,Response}
%%--------------------------------------------------------------------
etrn(Pid,Queue) -> gen_fsm:sync_send_event(Pid, {etrn,Queue}, infinity).

%%--------------------------------------------------------------------
%% Function: expn(Pid,Alias)
%%           Pid   = pid()
%%           Alias = String()
%% Descrip.: Expands and ALias to a list of email address. Usually not 
%%            implimented due to anti-spam practices
%% Returns : {Code,Response}
%%--------------------------------------------------------------------
expn(Pid,Alias) -> gen_fsm:sync_send_event(Pid, {expn,Alias}, infinity).

%%--------------------------------------------------------------------
%% Function: help(Pid,HostName)
%%           Pid      = pid()
%%           HostName = String() - Your Server Name
%% Descrip.: HELO handshake
%% Returns : {Code,Response} - {250,String}
%%--------------------------------------------------------------------
helo(Pid,HostName) -> gen_fsm:sync_send_event(Pid, {helo,HostName}, infinity).

%%--------------------------------------------------------------------
%% Function: help(Pid,Message)
%%           Pid = pid()
%% Descrip.: Asks for help about how to use the server
%% Returns : {Code,Response} - {221,String}
%%--------------------------------------------------------------------
help(Pid) -> gen_fsm:sync_send_event(Pid, help, infinity).

%%--------------------------------------------------------------------
%% Function: mail(Pid,From)
%%           Pid  = pid()
%%           From = String() - Address the email is from
%% Descrip.: Tell the server what the from address of the serer is
%% Returns : {Code,Response}
%%--------------------------------------------------------------------
mail(Pid,From) -> gen_fsm:sync_send_event(Pid, {mail,From}, infinity).

%%--------------------------------------------------------------------
%% Function: noop(Pid)
%%           Pid     = pid()
%% Descrip.: No Operation
%% Returns : {Code,Response}
%%--------------------------------------------------------------------
noop(Pid) -> gen_fsm:sync_send_event(Pid, noop, infinity).

%%--------------------------------------------------------------------
%% Function: quit(Pid)
%%           Pid     = pid()
%% Descrip.: Sends QUIT commadn and waits for response befor closing 
%%            the Socket
%% Returns : {Code,Response} - {250,String}
%%--------------------------------------------------------------------
quit(Pid) -> gen_fsm:sync_send_event(Pid, quit, infinity).

%%--------------------------------------------------------------------
%% Function: rcpt(Pid,Message)
%%           Pid = pid()
%%           To  = String() - Email Addres this message goes to
%% Descrip.: Tell server address of recipiant, usually max 100.
%% Returns : {Code,Response} - {250,"ok"}
%%--------------------------------------------------------------------
rcpt(Pid,To) -> gen_fsm:sync_send_event(Pid, {rcpt,To}, infinity).

%%--------------------------------------------------------------------
%% Function: rset(Pid)
%%           Pid     = pid()
%% Descrip.: Reset server state to jsut after HELO or EHLO command
%% Returns : {Code,Response}
%%--------------------------------------------------------------------
rset(Pid) -> gen_fsm:sync_send_event(Pid, rset).

%%--------------------------------------------------------------------
%% Function: vrfy(Pid,Address)
%%           Pid     = pid()
%%           Address = String() - Email Address
%% Descrip.: Tries to verify email address, this feature is usually 
%%            disabled to discourage email address harvesting
%% Returns : {Code,Response}
%%--------------------------------------------------------------------
vrfy(Pid,Address) -> gen_fsm:sync_send_event(Pid, {vrfy,Address}, infinity).

auth(Pid, User, Password) -> gen_fsm:sync_send_event(Pid, {auth, User, Password}, infinity).

stop(Pid) -> exit(Pid, kill).

%%--------------------------------------------------------------------
%% Function: sendmail(IPAddress,Host,From,To,Message)
%%           IPAddress = term() tuple, i.e {10,1,1,3}
%%           Port      = Integer() - Default 25
%%           Host      = String() - Your Server name
%%           From      = String() - From Email Address
%%           To        = String() - To Email Address
%%           Message   = String() - Full Email Mesage to send
%% Descrip.: Sends data of email message
%% Returns : ok
%%--------------------------------------------------------------------


sendmail(IPAddress, Port, Host, From, To, User, Password, Subject, Message, Options) ->
    ?D("111"),
    {ok, Pid}  = connect(IPAddress, Port, Options),
    ?D("222"),
    try
        ?D("333"),
        ehlo(Pid, Host),
        ?D("444"),
        auth(Pid, User, Password),
        ?D("555"),
        mail(Pid, From),
        ?D("666"),
        rcpt(Pid, To),
        ?D("777"),
        %% TODO more message format 
        ToStr = "To: " ++ To ++ ?CRLF,
        SubjectStr = "Subject: " ++ Subject ++ ?CRLF,
        Body = ToStr ++ SubjectStr ++ ?CRLF ++ Message,
        ?D(Body),
        data(Pid, Body),
        ?D("888"),
        quit(Pid)
    catch Type:Err ->
              ?D({Type, Err}),
              error_logger:format("~p:~p error: ~p/~p~n~p~n", [?MODULE, ?LINE, Type, Err,
                                                               erlang:get_stacktrace()]),
              stop(Pid) 
    end.
        

sendmail_test(From, To, User, Pass, Subjet, Content) ->
    sendmail("smtp.mail.yahoo.com", 25, "127.0.0.1", From, To, User, Pass, Subjet, Content, []).
