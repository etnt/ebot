-module(ebot_lib).

-export([log_in/2
         , pong/2
	 , join/2
	 , command/2
	 , say/3
	 , say/4
        ]).

-import(proplists, [get_value/2]).


%%----------------------------------------------------------------------
%% Function: pong/2
%% Purpose:  Send a pong answer with the right id
%% Args:     Sock = socket
%%           Id   = ping id to send back to the server
%% Returns:  ok
%%     or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
pong(Sock, Id)->
    Pong = "PONG " ++ Id,
    gen_tcp:send(Sock, Pong).

%%----------------------------------------------------------------------
%% join/2
%% Format an IRC join command: Used to join a discussion channel
%%----------------------------------------------------------------------
join(Sock, Channel) ->
    Command = "JOIN " ++ Channel,
    command(Sock, Command).

%%----------------------------------------------------------------------
%% say/3
%% Say something in the given channel
%%----------------------------------------------------------------------
say(Sock, Message, Channel) ->
    Command = lists:concat(["PRIVMSG ", Channel, " :", Message]),
    command(Sock, Command).

say(Sock, Message, Channel, Who) ->
    Command = lists:concat(["PRIVMSG ", Channel, " :", Who, ": ", Message]),
    command(Sock, Command).

%%----------------------------------------------------------------------
%% command/2
%% Send a command to the IRC server
%%----------------------------------------------------------------------
command(Sock, Command) ->
    %% Command1 = lists:concat(
%%  		 [
%%  		 %% ":KredBot!KredBot@rox-6917E37C.hq.kred "++
%%  		  "PRIVMSG "++
%%  		  "snape_"
%%  		  " :" ++
%%  		  "hi"
%%  		 ]),
    
  %  CompleteCmd = [Command, "\r\n"],
    CompleteCmd = [Command,"\r\n"],
  
    gen_tcp:send(Sock, "\r\n"), % FIXME: Workaround: The first message of a
                                % sequence does not seem to be received by the
                                % server ...
				% Sending a blank line to awake the line...
    gen_tcp:send(Sock, CompleteCmd).

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
    NickCommand = ["NICK ", Nickname, "\n"],
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
    case string:tokens(Result, "\n") of
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
    PassCommand = ["PASS ", Password, "\n"],
    gen_tcp:send(Sock, PassCommand).

%% Send the user information to terminate the log in phase
log_in_user(Sock, Nickname, Realname) ->
    UserCommand = lists:concat(["USER ", Nickname,
                                " dummy dummy :", Realname, "\n"]),
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
