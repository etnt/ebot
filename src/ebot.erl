%%% --------------------------------------------------------------------
%%% Created : 2 Nov 2011 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : A major rework of the manderlbot code, aiming for a lean
%%%           code base, easily included into any application.
%%%
%%% Manderlbot was created:
%%%   8 Sep 2001 by Mickaël Rémond <mickael.remond@erlang-fr.org>
%%% --------------------------------------------------------------------
-module(ebot).

-export([start_link/1
         , init/3
         , loop/1
         , pong/2
	 , join/2
	 , command/2
	 , say/3
	 , say/4
        ]).

-import(proplists, [get_value/2, get_value/3]).

-record(state, {
          socket,
          data,
          buffer=[],
          loginfo_fun={error_logger,info_msg},
          logerr_fun={error_logger,error_msg}
         }).

%%% --------------------------------------------------------------------
%%% API
%%% --------------------------------------------------------------------

%% Start the worker
start_link(Data) ->
    proc_lib:start_link(?MODULE, init, [self(), [], Data]).


%% Purpose:  Send a pong answer with the right id
pong(Sock, Id)->
    Pong = "PONG " ++ Id,
    gen_tcp:send(Sock, Pong).


%% Format an IRC join command: Used to join a discussion channel
join(Sock, Channel) ->
    Command = "JOIN " ++ Channel,
    command(Sock, Command).

%% Say something in the given channel
say(Sock, Message, Channel) ->
    Command = lists:concat(["PRIVMSG ", Channel, " :", Message]),
    command(Sock, Command).

say(Sock, Message, Channel, Who) ->
    Command = lists:concat(["PRIVMSG ", Channel, " :", Who, ": ", Message]),
    command(Sock, Command).

%% Send a command to the IRC server
command(Sock, Command) ->
    %% Workaround: The first message of a sequence does not seem
    %% to be received by the server...so we are sending a blank
    %% line to get ound it.
    gen_tcp:send(Sock, "\r\n"), 
    gen_tcp:send(Sock, [Command,"\r\n"]).


%%% --------------------------------------------------------------------
%%% THE WORKER
%%% --------------------------------------------------------------------
-define(SECONDS(Sec), Sec*1000).

init(Parent, _Options, Data) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    S = #state{},
    connect_loop(S#state{data=Data
                         ,loginfo_fun=get_value(loginfo_function,
                                                Data, S#state.loginfo_fun)
                         ,logerr_fun=get_value(logerror_function,
                                               Data, S#state.logerr_fun)
                        }).

connect_loop(State) ->
    connect_loop(State, ?SECONDS(1)).

%% Implement a simple connect backoff in case of connect failure.
connect_loop(State, Seconds) ->
    timer:sleep(Seconds),
    case sock_connect(State#state.data) of
        {ok, Sock} ->
            setup_loop(State, Sock);
        _ ->
            elog(State, "connect to ~s:~p failed!",
                 [get_value(server, State#state.data),
                  get_value(port, State#state.data)]),
            connect_loop(State, ?SECONDS(min(Seconds*2, 3600)))
    end.

setup_loop(State0, Sock) ->
    State = State0#state{socket = Sock},
    _Motd = log_in(Sock, State#state.data),
    join_channels(Sock, State#state.data),
    loop(State).

join_channels(Sock, Data) ->
    [join(Sock, Channel)
     || Channel <- get_value(channels, Data)].

loop(State) ->
    receive Msg -> ?MODULE:loop(x(Msg, State)) end.

x({tcp, _Socket, Data}, State) ->
    Buffer = State#state.buffer,
    List = Buffer ++ binary_to_list(Data),
    Rest = chomp(State, List),
    State#state{buffer=Rest};

x({tcp_closed, _Socket}, State) ->
    elog(State, "tcp connection to ~s:~p closed, reconnecting...~n",
         [get_value(server, State#state.data),
          get_value(port, State#state.data)]),
    connect_loop(State);

x({tcp_error, _Socket, Reason}, State) ->
    elog(State, "tcp connection error from ~s:~p , Reason = ~p~n",
         [get_value(server, State#state.data),
          get_value(port, State#state.data),
          Reason]),
    State;

x({From, Msg}, State) ->
    ilog(State, "got message: ~p~n",[Msg]),
    {Answer, NewState} = handle_msg(Msg, State),
    From ! {From, Answer},
    NewState;

x(What, State) ->
    ilog(State, "got unknown msg: ~p~n",[What]),
    State.

%% Deal with any control messages
handle_msg(_Msg, State) ->
    {"not implemented", State}.


chomp(State, Data) ->
    Pos = string:str(Data, "\r\n"),
    case Pos of
	0 -> Data; % not complete line
	_ ->
	    Line = string:substr(Data, 1, Pos-1),
	    Rest = string:substr(Data, Pos+2,
                                 string:len(Data) - (Pos-1)),
            line(State, Line),
            chomp(State, Rest)
    end.

line(#state{socket=Sock}, "PING "++Rest) ->
    pong(Sock, Rest);

line(#state{socket=Sock, data=Data} = State, Line) ->
    Ms = get_value(matchspecs, Data),
    lists:foreach(
      fun({Re,{Mod,Fun} = CallBack}) ->
              case re:run(Line, Re) of
                  {match,_} = Match ->
                      ilog(State, "~p(~p): line match: ~p~n",
                           [?MODULE,?LINE,CallBack]),
                      spawn(fun() ->
                                    Mod:Fun(Sock, Line, Match)
                            end);
                  _ ->
                      false
              end
      end, Ms).


%% No fuss, connect!
sock_connect(Data) ->
    gen_tcp:connect(get_value(server, Data),
                    get_value(port, Data),
                    [binary
                     ,{packet, 0}
                     ,{nodelay, true}
                     ,{keepalive, true}
                     ,{active, true}
                    ]).

ilog(#state{loginfo_fun = {M,F}}, Fs, As) ->
    M:F(Fs, As).

elog(#state{logerr_fun = {M,F}}, Fs, As) ->
    M:F(Fs, As).

%%% ---------------------------------------------------------------------
%%% L O G  I N  P H A S E
%%% ---------------------------------------------------------------------
log_in(Sock, Data) ->
    log_in_nick(Sock, Nick = get_value(nick, Data)),
    log_in_pong(Sock),
    log_in_pass(Sock, get_value(password, Data)),
    log_in_user(Sock, Nick, get_value(realname, Data)),
    Motd = wait_for_motd(5000),
    Motd.

log_in_nick(Sock, Nickname) ->
    NickCommand = ["NICK ", Nickname, "\r\n"],
    gen_tcp:send(Sock, NickCommand).

%% Some server send an initial ping after the nickname
%% to check the connection.
log_in_pong(Sock) ->
    Result = receive
		 {tcp, Sock, Data} ->
		     binary_to_list(Data)
	     after 3000 ->
		     binary_to_list(<<>>)
	     end,
    case string:tokens(Result, "\r\n") of
        [Tok|_] -> testPingPong(Sock, Tok);
        _       -> ok
    end.

%% Check if the incoming data is a server ping
%% If so, answer it and thus maintains the connection
testPingPong(Sock, Data) ->
    case string:substr(Data, 1, 4) of
	"PING" ->
	    Id = string:substr(Data, 6),
	    pong(Sock, Id);
	_Other ->
	    ok
    end.

%% If the IRC server is password protected,
%% this function send the needed password.
log_in_pass(Sock, Password) ->
    PassCommand = ["PASS ", Password, "\r\n"],
    gen_tcp:send(Sock, PassCommand).

%% Send the user information to terminate the log in phase
log_in_user(Sock, Nickname, Realname) ->
    UserCommand = lists:concat(["USER ", Nickname,
                                " dummy dummy :", Realname, "\r\n"]),
    gen_tcp:send(Sock, UserCommand).

%% This function gets the Message Of The Day that IRC servers usually
%% send after the connection (usage rules of the server)
wait_for_motd(Timeout) ->
    wait_for_motd(Timeout, []).
wait_for_motd(Timeout, Acc) ->
    receive
	{tcp, _Sock, Data} ->
	    wait_for_motd(Timeout, Acc ++ binary_to_list(Data))
    after Timeout ->
	    Acc
    end.
