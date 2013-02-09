%%%---------------------------------------------------------------------------------------
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
-module(popc).
-author('cao.xu@rytong.com').


-export([connect/2,
         connect/3,
         login/3,
         stat/1,
         list/1,
         list/2,
         delete/2,
         quit/1,
         retrieve/2]).

-export([connect/0,
         test/2]).
%% ------------------------------------------
%% Set up a connection to a POP3 server
%% ------------------------------------------


connect(IpAddress, Port) ->
    connect(IpAddress, Port, []).
connect(IpAddress, Port, Options) ->
    popc_fsm:start_link(IpAddress, Port, Options).

login(Pid, User, Password) ->
    gen_fsm:sync_send_event(Pid, {auth, User, Password}, infinity).

stat(Pid) ->
     gen_fsm:sync_send_event(Pid, stat, infinity).

list(Pid) ->
    gen_fsm:sync_send_event(Pid, list, infinity).

list(Pid, MessageId) ->
    gen_fsm:sync_send_event(Pid, {list, MessageId}, infinity).

retrieve(Pid, MessageId) ->
    gen_fsm:sync_send_event(Pid, {retr, MessageId}, infinity).


delete(Pid, MessageId) ->
    gen_fsm:sync_send_event(Pid, {dele, MessageId}, infinity).

quit(Pid) ->
    gen_fsm:sync_send_event(Pid, quit, infinity).
 



connect() -> connect("mail.rytong.com", 995, [ssl]).

test(User, Pass) ->
    {ok, P} = connect(),
    login(P, User, Pass),
    stat(P),
    list(P),
    list(P,1),
    retrieve(P, 1),
    %%delete(P,1),
    quit(P).
   
    

