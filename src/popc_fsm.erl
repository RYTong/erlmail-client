%%%---------------------------------------------------------------------------------------
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
-module(popc_fsm).
-author('cao.xu@rytong.com').

-include("client.hrl").
-behavior(gen_fsm). % Finite State Machine

%% gen_fsm callbacks
-export([init/1, 
         handle_event/3, 
         handle_sync_event/4,
         handle_info/3,
         terminate/3, 
         code_change/4]).

-export([start_link/3,
         'POPC_CMD'/2,
         'POPC_CMD'/3]).

-export([do_parse_test/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link(Host,Port, Options) -> gen_fsm:start_link(?MODULE, [Host,Port, Options], []).

'POPC_CMD'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'POPC_CMD', State, ?TIMEOUT}.

%% Authenticate with USER and PASS Command

'POPC_CMD'({auth, User, Password}, _From, #popc_fsm{socket = S,
                                                    socket_type = Type,
                                                    state = authorization
                                                    } = State) ->
    ?D({auth, User, Password}),
    UserCmd = "USER " ++ User,
    ok = request(S, Type, UserCmd),
    ?D(UserCmd),
    case response(S, Type) of
        {error, Err} ->
            {reply, {error,Err}, 'POPC_CMD', State};
        {ok, _} ->
            PassCmd = "PASS " ++ Password,
            ok = request(S, Type, PassCmd),
            ?D(PassCmd),
            case response(S, Type) of 
                {error, Err1} ->
                    {reply, {error,Err1}, 'POPC_CMD', State};
                {ok, _} ->
                    {reply, ok, 'POPC_CMD', State#popc_fsm{state=transaction}}
            end
    end;


%% LIST Command

'POPC_CMD'(list, _From, #popc_fsm{socket = S,
                                  socket_type = Type,
                                  state = transaction
                                 } = State) ->
    ?D({list, all}),
    ListCmd = "LIST",
    ok = request(S, Type, ListCmd),
    ?D(ListCmd),
    case response(S, Type) of
        {error, Err} ->
            {reply, {error,Err}, 'POPC_CMD', State};
        {ok, Data} ->  
            Res = parse_multi_line(Data, S, Type),
            {reply, Res, 'POPC_CMD', State}
    end;
'POPC_CMD'({list, MessageId}, _From, #popc_fsm{socket = S,
                                               socket_type = Type,
                                               state = transaction
                                              } = State) ->
    ?D({list, MessageId}),
    ListCmd = lists:concat(["LIST ", MessageId]),
    ok = request(S, Type, ListCmd),
    ?D(ListCmd),
    case response(S, Type) of
        {error, Err} ->
            {reply, {error,Err}, 'POPC_CMD', State};
        {ok, Data} -> 
            {reply, {ok, Data}, 'POPC_CMD', State}
    end;

%% RETRIEVE Command

'POPC_CMD'({retr, MessageId}, _From, #popc_fsm{socket = S,
                                               socket_type = Type,
                                               state = transaction
                                              } = State) ->
    ?D({retrieve, MessageId}),
    RetrCmd = lists:concat(["RETR ", MessageId]),
    ok = request(S, Type, RetrCmd),
    ?D(RetrCmd),
    case response(S, Type) of
        {error, Err} ->
            {reply, {error,Err}, 'POPC_CMD', State};
        {ok, Data} ->
             Res = parse_multi_line(Data, S, Type),
            {reply, Res, 'POPC_CMD', State}
    end;

%% DELE Command

'POPC_CMD'({dele, MessageId}, _From, #popc_fsm{socket = S,
                                  socket_type = Type
                                 } = State) ->
    ?D({dele, MessageId}),
    DeleCmd = lists:concat(["DELE ", MessageId]),
    ok = request(S, Type, DeleCmd),
    ?D(DeleCmd),
    case response(S, Type) of
        {error, Err} ->
            {reply, {error,Err}, 'POPC_CMD', State};
        {ok, Data} ->              
            {reply, Data, 'POPC_CMD', State}
    end;

%% DELE Command

'POPC_CMD'(quit, _From, #popc_fsm{socket = S,
                                  socket_type = Type
                                 } = State) ->
    ?D(quit),
    QuitCmd = "QUIT",
    request(S, Type, QuitCmd),
    ?D(QuitCmd),            
    {stop, normal, ok, State};

%% STAT Command

'POPC_CMD'(stat, _From, #popc_fsm{socket = S,
                                  socket_type = Type,
                                  state = transaction
                                 } = State) ->
    StatCmd = "STAT",
    ok = request(S, Type, StatCmd),
    ?D(StatCmd),
    case response(S, Type) of
        {error, Err} ->
            {reply, {error,Err}, 'POPC_CMD', State};
        {ok, Data} ->
            {reply, {ok, Data}, 'POPC_CMD', State}
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([Host, Port, Options]) ->
    ?D("Initializing POPC FSM ..."),
    process_flag(trap_exit, true),
    State = case lists:member(ssl, Options) of
                true ->
                    ssl:start(),
                    {ok,Socket} = ssl:connect(Host,Port,[binary,{packet,0},{active,once}],infinity),
                    #popc_fsm{socket = Socket, socket_type = ssl};
                _ ->
                    {ok,Socket} = gen_tcp:connect(Host,Port,[binary,{packet,0},{active,once}],infinity),
                    #popc_fsm{socket = Socket}
            end,
    case response(State#popc_fsm.socket, State#popc_fsm.socket_type) of
        {ok, _Data} ->
            {ok, 'POPC_CMD',State};
        {error, Err} -> 
            {stop, normal, Err}
    end.


handle_event(close, _AnyState, #popc_fsm{socket = S, socket_type = Type}) ->
    request(S, Type, "QUIT"),
    {stop, i_have_quit, []}.


handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.



handle_info({ssl, Socket, Bin}, StateName, #popc_fsm{socket=Socket, socket_type = ssl} = StateData) ->
    ssl:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({ssl_closed, Socket}, _StateName, #popc_fsm{socket=Socket, socket_type = ssl} = StateData) ->
    ?D("ssl closed by remote server"),
    {stop, normal, StateData};

handle_info({tcp, Socket, Bin}, StateName, #popc_fsm{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName, #popc_fsm{socket=Socket} = StateData) ->
    ?D("tcp closed by remote server"),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    ?D(_Info),
    {next_state, StateName, StateData}.

terminate(Reason,_StateName,#popc_fsm{socket=Socket, socket_type = ssl}) ->
    ssl:close(Socket),
    {terminated, Reason};
terminate(Reason,_StateName,#popc_fsm{socket=Socket}) -> 
    gen_tcp:close(Socket),
    {terminated, Reason}.
    
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

request(Socket, ssl, Msg) ->
    ssl:send(Socket, Msg ++ ?CRLF);
request(Socket, _, Msg) ->
    gen_tcp:send(Socket, Msg ++ ?CRLF).


response(Socket, Type) ->
    case recv(Socket, Type) of
        {error, _} = Err -> Err;
        Data -> parse_response(Data)
    end.

parse_response("+OK" ++ T) ->
    {ok, T};
parse_response("-ERR" ++ T) ->
    {error, T};
parse_response(E) ->
    {error, E}.


recv(S, Type) ->
    Tag = case Type of 
              ssl -> ssl;
              _ -> tcp
          end,
    receive
        {Tag, S, Bin} ->
            ?D(Bin),
            case Tag of
                ssl -> ssl:setopts(S, [{active,once}, binary]);
                _ -> inet:setopts(S, [{active,once}, binary])
            end,
            binary_to_list(Bin);
        Err ->
            ?D(Err),
            {error, Err}
        after ?TIMEOUT ->
            {error, timeout}
    end.

%% Handle byte-stuff here.
%% @link http://tools.ietf.org/html/rfc1939#section-3

%% RFC1939 
%% Responses to certain commands are multi-line.  In these cases, which
%%    are clearly indicated below, after sending the first line of the
%%    response and a CRLF, any additional lines are sent, each terminated
%%    by a CRLF pair.  When all lines of the response have been sent, a
%%    final line is sent, consisting of a termination octet (decimal code
%%    046, ".") and a CRLF pair.  If any line of the multi-line response
%%    begins with the termination octet, the line is "byte-stuffed" by
%%    pre-pending the termination octet to that line of the response.
%%    Hence a multi-line response is terminated with the five octets
%%    "CRLF.CRLF".  When examining a multi-line response, the client checks
%%    to see if the line begins with the termination octet.  If so and if
%%    octets other than CRLF follow, the first octet of the line (the
%%    termination octet) is stripped away.  If so and if CRLF immediately
%%    follows the termination character, then the response from the POP
%%    server is ended and the line containing ".CRLF" is not considered
%%    part of the multi-line response.

%% Return {ok, String} | {error, timeout}
parse_multi_line(Data, S, Type) ->
    do_parse(0, Data, [], S, Type).

%% STATE - 0 begin of the line
%%       - 1 rest of the line



do_parse(0, [$., 13, 10|_], Res, _S, _Type) ->
    {ok, lists:reverse(Res)};

do_parse(0, [$.|T], Res, S, Type) ->
    do_parse(1, T, Res, S, Type);


do_parse(_, [13, 10|T], Res, S, Type) ->
    do_parse(0, T, [10, 13|Res], S, Type);

do_parse(_, [H|T], Res, S, Type) ->
    do_parse(1, T, [H|Res], S, Type);

do_parse(_State, {error, Err}, _Res, _S, _Type) ->
    {error, Err};
%% %% for test
%% do_parse(_State, [], _Res, undefined, _Type) ->
%%    {error, "socket is undefined"};

do_parse(State, [], Res, S, Type) ->
    do_parse(State, recv(S, Type), Res, S, Type).

do_parse_test() ->
    Str = "></HTML>\r\n..\r\n------=_001_NextPart344028817884_=------\r\n\r\n.\r\n",
    parse_multi_line(Str, undefined, undefined).

