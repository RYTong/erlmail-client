%% Copyright (c) 2009-2010 Beijing RYTong Information Technologies, Ltd.
%% All rights reserved.
%%
%% No part of this source code may be copied, used, or modified
%% without the express written consent of RYTong.
-module(socket_util).

%%
%% Include files
%%
-include("client.hrl").

%%
%% Exported Functions
%%
-export([connect/3,
         read/1,
         write/2,
         close/1,
         setopts/2]).

%%
%% API Functions
%%

connect(Host,Port, Options) ->
    {UseSsl, Timeout} = get_options(Options),
    if UseSsl ->
           ssl:start(),
           {ok, Socket} = ssl:connect(Host,Port,[binary,{packet,0},{active,once}],Timeout),
           #socket{fd = Socket,
                   type = ssl,
                   timeout = Timeout};
       true ->
           {ok, Socket} = gen_tcp:connect(Host,Port,[binary,{packet,0},{active,once}],Timeout),
           #socket{fd = Socket,
                   type = tcp,
                   timeout = Timeout}
    end.

write(#socket{fd = S, 
              type = Type}, Msg) ->  
    Last = string:right(Msg,2),
    Body = case Last of
               ?CRLF -> Msg;
               _     -> Msg ++ ?CRLF
           end,
    case Type of
        ssl -> ssl:send(S, Body);
        _ -> gen_tcp:send(S, Body)
    end.

read(#socket{fd = S, 
             type = Type,
             timeout = T} = Socket) ->
    ?D(Socket),
    Tag = case Type of 
              ssl -> ssl;
              _ -> tcp
          end,
    receive
        {Tag, S, Bin} ->
            ?D(Bin),
            setopts(Socket, [{active,once}, binary]),
            binary_to_list(Bin);
        Err ->
            ?D(Err),
            {error, Err}
        after T ->
            {error, timeout}
    end.

close(#socket{fd = S, type = ssl}) ->
    ssl:close(S);
close(#socket{fd = S}) ->
    gen_tcp:close(S).

setopts(#socket{fd = S, type = ssl}, Options) ->
    ssl:setopts(S, Options);
setopts(#socket{fd = S}, Options) ->
    inet:setopts(S, Options).

%%
%% Local Functions
%%

%% Now we only support ssl and timeout as options.
%% Input as [ssl, {timeout, 60000}] will gen output {true, 60000}.
get_options(Options) ->
    do_get_options(Options, {false, ?TIMEOUT}).

do_get_options([], Res) ->
    Res;
do_get_options([ssl|T], {_, Timeout}) ->
    do_get_options(T, {true, Timeout});
do_get_options([{timeout, Timeout}|T], {Ssl, _}) ->
    do_get_options(T, {Ssl, Timeout});
do_get_options([_H|T], Res) ->
    do_get_options(T, Res).

