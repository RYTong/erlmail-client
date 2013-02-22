%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       SMTP Client FSM for ErlMail
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
-module(smtpc_fsm).
-author('sjackson@simpleenigma.com').
-behavior(gen_fsm). % Finite State Machine
-include("client.hrl").

%% External Exports
-export([start/1,
         start/3,
         start_link/1,
         start_link/3]).

%% API functions
-export([]).
%% states
-export([smtp_cmd/3]).

%% Testing Functions

-export([]).
%% gen_server callbacks
-export([init/1, 
         handle_event/3, 
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

-import(socket_util, [connect/3,
                      write/2,
                      close/1]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
% Manual Start
start(Host)      -> start(Host,25, []).
start(Host,Port, Options) -> gen_fsm:start(?MODULE, [Host,Port, Options], []).

% Supervised Start
start_link(Host)      -> start_link(Host,25, []).
start_link(Host,Port, Options) -> gen_fsm:start_link(?MODULE, [Host,Port, Options], []).

%%%----------------------------------------------------------------------
%%% State functions
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% HELO Command
%%%----------------------------------------------------------------------
smtp_cmd({helo, Name}, From, State)->
    Msg = "HELO" ++ [32] ++ Name,
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {250, Resp} -> 
            gen_fsm:reply(From,{250,Resp}),
            {next_state,smtp_cmd,State#smtpc{state=mail}};
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% EHLO Command
%%%----------------------------------------------------------------------
smtp_cmd({ehlo, Name}, From, State) ->
    Msg = "EHLO" ++ [32] ++ Name,
    ?D(Msg),
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {250, Resp} ->
            ?D({250, Resp}),
            NewState = lists:foldl(fun("250-AUTH=" ++ Rest, Acc) ->
                                           AuthTypes = string:tokens(Rest, " "),
                                           Acc#smtpc{auth = AuthTypes, state = auth};
                                      ("250-AUTH" ++ Rest, Acc) ->
                                           AuthTypes = string:tokens(Rest, " "),
                                           Acc#smtpc{auth = AuthTypes, state = auth};
                                      ("250"++ Feature, Acc) ->
                                           Features = Acc#smtpc.features,
                                           Acc#smtpc{features = [string:sub_string(Feature, 2)|Features]};
                                      (_Other, Acc) ->
                                           Acc
                                   end, State#smtpc{state = mail}, string:tokens(Resp, "\r\n")),
            ?D(NewState),
            gen_fsm:reply(From,{250,Resp}),
            {next_state, smtp_cmd, NewState};
        {Code, Resp} -> 
            ?D({Code, Resp}),
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            ?D(Error),
            gen_fsm:reply(From,{error,Error}),
            {stop, Error, [], []}
    end;

%%%----------------------------------------------------------------------
%%% AUTH Command
%%%----------------------------------------------------------------------
smtp_cmd({auth, User, Password}, From, State=#smtpc{auth = AuthTypes, state = auth}) ->
    AuthType = type(AuthTypes),
    Res = case AuthType of
              'plain' -> auth_plain(User, Password, State#smtpc.socket);
              'login' -> auth_login(User, Password, State#smtpc.socket);
              _ -> throw("auth type not supported")
          end,    
    case Res of
        ok -> 
            {reply,ok,smtp_cmd, State#smtpc{state = mail}};
        Error -> 
            ?D(Error),
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;



%%%----------------------------------------------------------------------
%%% ETRN Command
%%%----------------------------------------------------------------------
smtp_cmd({etrn, Queue}, From, State) ->
    Msg = "ETRN" ++ [32] ++ Queue,
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {250, Resp} -> 
            gen_fsm:reply(From,{250,Resp}),
            {next_state, smtp_cmd, State};
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error,Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% ETRN Command
%%%----------------------------------------------------------------------
smtp_cmd({expn, Alias}, From, State) ->
    Msg = "EXPN" ++ [32] ++ Alias,
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {250, Resp} -> 
            gen_fsm:reply(From,{250,Resp}),
            {next_state, smtp_cmd, State};
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% HELP Command
%%%----------------------------------------------------------------------
smtp_cmd(help, From, State) ->
    Msg = "HELP",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {214, Resp} -> 
            gen_fsm:reply(From,{214,Resp}),
            {next_state, smtp_cmd, State};
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% MAIL Command
%%%----------------------------------------------------------------------
smtp_cmd({mail, Address}, From, State) ->
    Msg = "MAIL FROM:<" ++ Address ++ ">",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {250, Resp} -> 
            gen_fsm:reply(From,{250,Resp}),
            {next_state, smtp_cmd, State#smtpc{state=rcpt}};
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% NOOP Command
%%%----------------------------------------------------------------------
smtp_cmd(noop, From, State) ->
    Msg = "NOOP",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {250, Resp} -> 
            gen_fsm:reply(From,{250,Resp}),
            {next_state, smtp_cmd, State};
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% QUIT Command
%%%----------------------------------------------------------------------
smtp_cmd(quit, From, State) ->
    Msg = "QUIT",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {221, Resp} -> 
            ?D(Resp),
            gen_fsm:reply(From,{221,Resp}),
            {next_state, smtp_cmd, State};
        {Code, Resp} -> 
            ?D({Code, Resp}),
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            ?D(Error),
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% RSET Command
%%%----------------------------------------------------------------------
smtp_cmd(rset, From, State) ->
    Msg = "RSET",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {250, Resp} -> 
            gen_fsm:reply(From,{250,Resp}),
            {next_state, smtp_cmd, State#smtpc{state=mail}};
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% RCPT Command
%%%----------------------------------------------------------------------
smtp_cmd({rcpt, Address}, From, State) ->
    Msg = "RCPT TO:<" ++ Address ++ ">",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {250, Resp} -> 
            gen_fsm:reply(From,{250,Resp}),
            {next_state, smtp_cmd, State#smtpc{state=rcpt}};
        {511, Resp} -> 
            gen_fsm:reply(From,{511,Resp}),
            {next_state, smtp_cmd, State#smtpc{state=rcpt}};
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% VRFY Command
%%%----------------------------------------------------------------------
smtp_cmd({vrfy, Address}, From, State) ->
    Msg = "VRFY " ++ Address,
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {250, Resp} -> 
            gen_fsm:reply(From,{250,Resp}),
            {next_state, smtp_cmd, State#smtpc{state=rcpt}};
        {511, Resp} -> 
            gen_fsm:reply(From,{511,Resp}),
            {next_state, smtp_cmd, State#smtpc{state=rcpt}};
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% DATA Command
%%%----------------------------------------------------------------------
smtp_cmd({data,Message}, From, State) ->
    ?D(Message),
    Msg = "DATA",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
        {354, _Resp} -> 
            write(State#smtpc.socket, Message ++ ?SMTP_DATA_END),
            case read(State#smtpc.socket) of
                {250, DataResp} -> 
                    gen_fsm:reply(From,{250,DataResp}),
                    {next_state, smtp_cmd, State#smtpc{state=mail}};
                {Code, DataResp} -> 
                    gen_fsm:reply(From,{Code,DataResp}),
                    {next_state,smtp_cmd, State};
                Error -> 
                    gen_fsm:reply(From,{error, Error}),
                    {stop, Error, [], []}
            end;
        {Code, Resp} -> 
            gen_fsm:reply(From,{Code,Resp}),
            {next_state,smtp_cmd, State};
        Error -> 
            gen_fsm:reply(From,{error, Error}),
            {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% INFO Command 
%%%----------------------------------------------------------------------
smtp_cmd({info,Info}, From, State) -> 
    case Info of
        all -> gen_fsm:reply(From,State);
        features -> gen_fsm:reply(From,State#smtpc.features);
        state -> gen_fsm:reply(From,State#smtpc.state);
        type -> gen_fsm:reply(From,State#smtpc.type);
        scoket -> gen_fsm:reply(From,State#smtpc.socket);
        _Other -> gen_fsm:reply(From,State)
    end,
    {next_state,smtp_cmd,State}.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([Server,Port, Options]) ->
    Socket = connect(Server,Port,Options),
    case read(Socket) of
        {220, Resp} -> {ok, smtp_cmd, #smtpc{socket=Socket,type=smtp_type(Resp)}};
        {error,Reason} -> {error,Reason}
    end.


handle_event(close, _AnyState, #popc_fsm{socket = S}) ->
    write(S, "QUIT"),
    {stop, i_have_quit, []}.

handle_sync_event(rset, _From, _AnyState, State) ->
    {reply, ok, smtp_cmd, State}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason,_StateName, #smtpc{socket = S}) -> 
    close(S),
    {terminated, Reason}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: parse(Line)
%%           Line: List - one line of SMTP data
%% Descrip.: Separates a line of SMTP data into a command and the data
%% Returns : {Command,Response}
%%--------------------------------------------------------------------

read(Socket) ->
    case socket_util:read(Socket) of
        {error, _} =Err -> Err;
        Data -> parse(Data)
    end.

parse(Line) ->
    {CodeString,RespText} = lists:split(3,Line),
    {list_to_integer(CodeString),string:strip(RespText)}.

smtp_type(RespText) ->
    case string:str(http_util:to_lower(RespText),"esmtp") of
        0 -> smtp;
        _ -> esmtp
    end.



type(AuthTypes) ->
    case lists:member("PLAIN", AuthTypes) of
        true -> 'plain';
        _ ->  
            case lists:member("LOGIN", AuthTypes) of
                true -> 'login';
                _ -> other
            end
    end.              

auth_plain(User, Password, Socket) ->
    Plain = User ++ [0] ++ User ++ [0] ++ Password,
    Msg = "AUTH PLAIN" ++ [32] ++  binary_to_list(base64:encode(Plain)),
    ?D(Msg),
    write(Socket, Msg),
    case read(Socket) of
        {235, Resp} ->
            ?D(Resp),
            ok;
        %% {530, "Access Denied"} when User or Password is wrong
        Err ->
            ?D(Err),
            {error, Err}
    end.

auth_login(User, Password, Socket) ->
    Login = "AUTH LOGIN",
    write(Socket, Login),
    case read(Socket) of
        {334, "VXNlcm5hbWU6\r\n"} ->
            U = base64:encode(User),
            ?D({user, User, U}),
            write(Socket, binary_to_list(U)),
            case read(Socket) of
                {334, "UGFzc3dvcmQ6\r\n"} ->
                    P = base64:encode(Password),
                    write(Socket, binary_to_list(P)),
                    case read(Socket) of
                        {235, Resp} ->
                            ?D(Resp),
                            ok;
                        Err ->
                            {error,Err}
                    end;
                Err1 ->
                    {error,Err1}
            end;
        Err2 ->
            {error,Err2}
    end.


